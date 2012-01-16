{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HEP.ROOT.Plot (
    -- * AST
    Command(..)
  , Plot(..)
  , Option(..)
  , Axis(..)
  , Legend(..)
  , Color(..)
    -- * Sending commands
  , renderCommand
  , draw
  , draws
  ) where

import Control.Applicative
import Control.Exception
import Data.Histogram.Generic (Histogram)
import Network.Socket

import System.Directory    (canonicalizePath)
import System.Environment
import System.Posix.User
import Text.Printf

import Prelude hiding (catch)



----------------------------------------------------------------
-- AST
----------------------------------------------------------------

-- | Top level command
data Command =
    Clear
  | Exit
  | Save   String
  | Legend Legend
  | Set    Option
  | Plot   Plot
  | Add    Plot

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
    -- | Silence state
    Silent Toggle
    -- | Title of plot
  | Title  String
    -- | Width of line
  | LineWidth Int
    -- | Color of line (with enum)
  | LineColor  Color
    -- | Color of line (with int)
  | LineColorI Int
    -- | Fill color (with enum)
  | FillColor  Color
    -- | Fill color (with int)
  | FillColorI Int
    -- | Histogram options
  | HistOpt    HistOpt
    -- | X axis
  | XAxis Axis
    -- | Y axis
  | YAxis Axis

-- | Axis parameters
data Axis
    -- ^ Axis label
  = Label String
    -- ^ No label
  | NoLabel
    -- ^ Set log scale
  | LogScale Toggle
    -- ^ Set range
  | Range Double Double
    -- ^ Set automatic range
  | RangeAuto

-- | Histogram options
data HistOpt =
    -- | Draw text
    HistText
  | HistTextF Toggle
    -- | Draw colored
  | HistBox
  | HistBoxF Toggle
    -- | Draw colored
  | HistColor
  | HistColorF Toggle
    -- | Scatter plot
  | HistScatter
  | HistScatterF Toggle
    -- | Contour plot
  | HistContour
  | HistContourN Int

data Legend =
    -- | Add new legend to the plot
    NewL (Double,Double) (Double,Double)
    -- | Delete legend from the plot
  | DeleteL
    -- | Add label to the last item in the stack
  | LegendStr   String
    -- | Add label to the item
  | LegendLabel String

-- | Toggle options on and off
data Toggle = ON
            | OFF

data Color =
    WHITE
  | BLACK
  | RED
  | GREEN
  | BLUE
  | YELLOW
  | MAGENTA
  | CYAN
  | FOREST
  | VIOLET
  deriving (Show,Eq,Enum)

-- | Convert command to the string
renderCommand :: Command -> IO String
renderCommand Clear      = return $ "clear"
renderCommand Exit       = return $ "exit"
renderCommand (Save nm)  = ("save " ++) <$> canonicalizePath nm
renderCommand (Set opt)  = return $ "set  " ++ renderOption opt
renderCommand (Plot pl)  = return $ "plot " ++ renderPlot pl
renderCommand (Add  pl)  = return $ "add  " ++ renderPlot pl
renderCommand (Legend l) = return $ "legend " ++ renderLegend l

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
renderOption (Silent o)     = "silent " ++ toggle o
renderOption (Title  t)     = "title " ++ show t
renderOption (LineWidth i)  = "line width " ++ show i
renderOption (LineColor  c) = renderOption $ LineColorI $ fromEnum c
renderOption (LineColorI i) = "line color " ++ show i
renderOption (FillColor  c) = renderOption $ FillColorI $ fromEnum c
renderOption (FillColorI i) = "fill color " ++ show i
renderOption (HistOpt o )   = "hist " ++ renderHistOpt o
renderOption (XAxis   a )   = "xaxis " ++ renderAxis a
renderOption (YAxis   a )   = "yaxis " ++ renderAxis a

-- Axis
renderAxis :: Axis -> String
renderAxis (Label str)  = "label " ++ show str
renderAxis  NoLabel     = "label -"
renderAxis (LogScale t) = "log " ++ toggle t
renderAxis (Range a b)  = "range " ++ show a ++ " " ++ show b
renderAxis  RangeAuto   = "range -"

-- Histogram options
renderHistOpt :: HistOpt -> String
renderHistOpt  HistText        = "text"
renderHistOpt (HistTextF o)    = "text " ++ toggle o
renderHistOpt  HistBox         = "box"
renderHistOpt (HistBoxF o)     = "box " ++ toggle o
renderHistOpt  HistColor       = "color"
renderHistOpt (HistColorF o)   = "color " ++ toggle o
renderHistOpt  HistScatter     = "scattter"
renderHistOpt (HistScatterF o) = "scattter" ++ toggle o
renderHistOpt  HistContour     = "contour"
renderHistOpt (HistContourN n) = "contour" ++ show n

-- Legend subcommand
renderLegend :: Legend -> String
renderLegend (NewL (x1,y1) (x2,y2)) = printf "add %g %g %g %g" x1 y1 x2 y2
renderLegend DeleteL                = "-"
renderLegend (LegendStr   s)        = "add "       ++ show s
renderLegend (LegendLabel s)        = "add label " ++ show s

toggle :: Toggle -> String
toggle ON  = "on"
toggle OFF = "off"


----------------------------------------------------------------
-- Communicate with user
----------------------------------------------------------------

-- | Send list of command to rt-plot
draw :: [Command] -> IO ()
draw cmds = do
  tmpdir <- catch (getEnv "TMPDIR") (\(_ :: SomeException) -> return "/tmp")
  uname  <- userName <$> (getUserEntryForID =<< getRealUserID)
  let sock = tmpdir ++ "/" ++ uname ++ "/rt-socket"
  -- Send data
  bracket (socket AF_UNIX Stream defaultProtocol) (sClose) $ \s -> do
    connect s (SockAddrUnix sock)
    -- Send data
    let sendAll str = do
          n <- send s str
          case drop n str of
            [] -> return ()
            xs -> sendAll xs
    sendAll . unlines =<< mapM renderCommand cmds


-- | Send list of commands surrounded in silent on/off marks
draws :: [Command] -> IO ()
draws cmds = draw $ [Set (Silent ON)] ++ cmds ++ [Set (Silent OFF)]