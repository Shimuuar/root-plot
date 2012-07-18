{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HEP.ROOT.Plot (
    -- * AST
    Cmd
  , Command
  , Plot(..)
  , Color(..)
  , Toggle(..)
    -- * Commands
  , clear
  , exit
  , save
  , legend
  , add
  , plot
  , set
    -- ** Set
  , silent
  , title
  , lineWidth
  , lineColor
  , lineStyle
  , markerStyle
  , fillColor
  , histOpt
  , xaxis
  , yaxis
  , zaxis
    -- ** Axis
  , label
  , noLabel
  , logScale
  , rangeAB
  , rangeAuto
    -- ** Histogram options
  , histText
  , histBox
  , histColor
  , histScatter
  , histContour
  , histPalette
    -- ** Legend
  , newLegend
  , deleteLegend
  , legendStr
  , legendLabel
    -- * Sending commands
  , renderCommand
  , sendCommands
  , draws
  ) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer
import Control.Exception
import Control.Concurrent         (threadDelay)
import Network.Socket
import Network.Socket.ByteString.Lazy (sendAll)
import Blaze.ByteString.Builder       (toLazyByteString)

import System.Environment
import System.Posix.User

import HEP.ROOT.Plot.AST

import Prelude hiding (catch)


----------------------------------------------------------------
--
----------------------------------------------------------------
type Cmd a = Writer [a] ()

cmd :: a -> Cmd a
cmd = tell . (:[])

cmds :: (a -> b) -> Cmd a -> Cmd b
cmds f = mapWriter $ second $ map f


----------------------------------------------------------------
-- Commands
----------------------------------------------------------------

clear :: Cmd Command
clear = cmd Clear

exit :: Cmd Command
exit = cmd Exit

save :: String -> Cmd Command
save = cmd . Save

legend :: Cmd Legend -> Cmd Command
legend = cmds Legend

add :: Plot -> Cmd Command
add = cmd . Add

plot :: Plot -> Cmd Command
plot = cmd . Plot

set :: Cmd Option -> Cmd Command
set = cmds Set


----------------------------------------------------------------
-- Set command
----------------------------------------------------------------

silent :: Toggle -> Cmd Option
silent = cmd . Silent

title :: String -> Cmd Option
title = cmd . Title


lineWidth :: Int -> Cmd Option
lineWidth = cmd . LineWidth

lineColor :: Color -> Cmd Option
lineColor = cmd . LineColor

lineStyle :: String -> Cmd Option
lineStyle = cmd . LineStyle

markerStyle :: String -> Cmd Option
markerStyle = cmd . MarkerStyle

fillColor :: Color -> Cmd Option
fillColor = cmd . FillColor

histOpt :: Cmd HistOpt -> Cmd Option
histOpt = cmds HistOpt

xaxis :: Cmd Axis -> Cmd Option
xaxis = cmds XAxis

yaxis :: Cmd Axis -> Cmd Option
yaxis = cmds YAxis

zaxis :: Cmd Axis -> Cmd Option
zaxis = cmds ZAxis

----------------------------------------------------------------
-- Axis
----------------------------------------------------------------

label :: String -> Cmd Axis
label = cmd . Label

noLabel :: Cmd Axis
noLabel = cmd NoLabel

logScale :: Toggle -> Cmd Axis
logScale = cmd . LogScale

rangeAB :: Double -> Double -> Cmd Axis
rangeAB a b = cmd $ Range a b

rangeAuto :: Cmd Axis
rangeAuto = cmd RangeAuto


----------------------------------------------------------------
-- Histogram options
----------------------------------------------------------------

histText :: Toggle -> Cmd HistOpt
histText = cmd . HistText

histBox :: Toggle -> Cmd HistOpt
histBox = cmd . HistBox

histColor :: Toggle -> Cmd HistOpt
histColor = cmd . HistColor

histScatter :: Toggle -> Cmd HistOpt
histScatter = cmd . HistScatter

histContour :: Int -> Cmd HistOpt
histContour = cmd . HistContour

histPalette :: Toggle -> Cmd HistOpt
histPalette = cmd . HistPalette


----------------------------------------------------------------
-- Legend
----------------------------------------------------------------

newLegend :: (Double, Double) -> (Double, Double) -> Cmd Legend
newLegend a b = cmd $ NewL a b

deleteLegend :: Cmd Legend
deleteLegend = cmd DeleteL

legendStr :: String -> Cmd Legend
legendStr = cmd . LegendStr

legendLabel :: String -> Cmd Legend
legendLabel = cmd . LegendLabel


----------------------------------------------------------------
-- Communicate with user
----------------------------------------------------------------

-- | Send list of commands surrounded with silent on/off marks and leading clear command
draws :: Cmd Command -> IO ()
draws commands
  = sendCommands $ [Set (Silent ON), Clear] ++ execWriter commands ++ [Set (Silent OFF)]


-- | Send list of command to rt-plot
sendCommands :: [Command] -> IO ()
sendCommands commands = do
  tmpdir <- catch (getEnv "TMPDIR") (\(_ :: SomeException) -> return "/tmp")
  uname  <- userName <$> (getUserEntryForID =<< getRealUserID)
  let sock = tmpdir ++ "/" ++ uname ++ "/rt-socket"
  -- Send data
  bracket (socket AF_UNIX Stream defaultProtocol) (sClose) $ \s -> do
    forceConnect s (SockAddrUnix sock)
    -- Send data
    mapM_ (sendAll s . toLazyByteString) =<< mapM renderCommand commands


-- Connect forcefully
forceConnect :: Socket -> SockAddr -> IO ()
forceConnect s addr
  = connect s addr `catch` handler
  where
    handler :: IOException -> IO ()
    handler e
      -- If we there isn't any available connections wait for 100ms
      -- and try again It's VERY ugly but works.
      | show e == msg = threadDelay 100000 >> forceConnect s addr
      | otherwise     = throw e
    msg = "connect: resource exhausted (Resource temporarily unavailable)"
