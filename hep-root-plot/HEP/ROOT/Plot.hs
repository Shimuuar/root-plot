{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HEP.ROOT.Plot (
    -- * AST
    Command(..)
  , Plot(..)
  , Option(..)
    -- * Sending commands
  , renderCommand
  , draw
  , draws
  ) where

import Control.Applicative
import Control.Exception
import Data.Histogram.Generic
import Network.Socket

import System.Environment
import System.Posix.User

import Prelude hiding (catch)



----------------------------------------------------------------
-- AST
----------------------------------------------------------------

-- | Top level command
data Command =
    Clear
  | Exit
  | Set  Option
  | Plot Plot
  | Add  Plot

-- | Plot subcommand
data Plot where
  -- | Simple graph
  Graph    :: [(Double,Double)] -> Plot
  -- | Sequence of points
  Graph1   :: [Double]          -> Plot
  -- | Plot function
  Function :: (Double,Double) -> (Double -> Double) -> Plot
  -- | Plot histogram
  Hist     :: Show (Histogram v bin a) => Histogram v bin a -> Plot
  -- | Vertical line
  VLine    :: Double -> Plot
  -- | Horizontal line
  HLine    :: Double -> Plot

-- | Set subcommand
data Option =
    Silent Toggle
  | Title  String
  | LineWidth Int
  | LineColorI Int

-- | Toggle options on and off
data Toggle = ON
            | OFF

-- | Convert command to the string
renderCommand :: Command -> String
renderCommand Clear     = "clear"
renderCommand Exit      = "exit"
renderCommand (Set opt) = "set  " ++ renderOption opt
renderCommand (Plot pl) = "plot " ++ renderPlot pl
renderCommand (Add  pl) = "add  " ++ renderPlot pl

-- plot subcommand
renderPlot :: Plot -> String
renderPlot (Graph vals) =
  unlines $ ("graph -" : map (\(x,y) -> show x ++ "\t" ++ show y) vals) ++ ["<<<"]
renderPlot (Graph1 ys ) =
  unlines $ ("graph -" : map show ys) ++ ["<<<"]
renderPlot (Function (a,b) f) =
  renderPlot $ Graph [ (x, f x)
                     | i <- [0 .. n]
                     , let x = a + (b - a) * fromIntegral i / fromIntegral n
                     ]
  where n = 128 :: Int
renderPlot (Hist  h   ) =
  unlines [ "hist -"
          , show h
          , "<<<"
          ]
renderPlot (VLine x) = "vline " ++ show x
renderPlot (HLine x) = "hline " ++ show x

-- Option subcommand
renderOption :: Option -> String
renderOption (Silent ON)    = "silent on"
renderOption (Silent OFF)   = "silent off"
renderOption (Title  t)     = "title " ++ show t
renderOption (LineWidth i)  = "line width " ++ show i
renderOption (LineColorI i) = "line color " ++ show i


----------------------------------------------------------------
-- Communicate with user
----------------------------------------------------------------

-- | Send list of command to rt-plot
draw :: [Command] -> IO ()
draw cmds = do
  tmpdir <- catch (getEnv "TMPDIR") (\(e :: SomeException) -> return "/tmp")
  uname  <- userName <$> (getUserEntryForID =<< getRealUserID)
  let sock = tmpdir ++ "/" ++ uname ++ "/rt-socket"
  -- Send data
  bracket (socket AF_UNIX Stream defaultProtocol) (sClose) $ \s -> do
    connect s (SockAddrUnix sock)
    -- Send data
    let sendAll s str = do
          n <- send s str
          case drop n str of
            [] -> return ()
            xs -> sendAll s xs
    sendAll s (unlines $ map renderCommand cmds)


-- | Send list of commands surrounded in silent on/off marks
draws :: [Command] -> IO ()
draws cmds = draw $ [Set (Silent ON)] ++ cmds ++ [Set (Silent OFF)]