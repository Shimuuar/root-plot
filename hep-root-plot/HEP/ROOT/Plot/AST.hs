{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HEP.ROOT.Plot.AST (
    -- * AST
    Command(..)
  , Plot(..)
  , Option(..)
  , HistOpt(..)
  , Axis(..)
  , Legend(..)
  , Color(..)
  , Toggle(..)
    -- * Rendering commands
  , renderCommand
  ) where

import Data.Histogram.Generic (Histogram)
import System.Directory       (getCurrentDirectory,makeRelativeToCurrentDirectory)
import Text.Printf



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
  Graph     :: [(Double,Double)] -> Plot
  -- | Sequence of points
  Graph1    :: [Double]          -> Plot
  -- | Polygon
  Polygon   :: [(Double,Double)] -> Plot
  -- | Plot function
  Function  :: (Double,Double) -> (Double -> Double) -> Plot
  -- | Plot function using many points at same time
  FunctionN :: Int -> (Double,Double) -> (Double -> Double) -> Plot
  -- | Plot histogram
  Hist      :: Show (Histogram v bin a) => Histogram v bin a -> Plot
  -- | Vertical line
  VLine     :: Double -> Plot
  -- | Horizontal line
  HLine     :: Double -> Plot
  -- | Vertical band
  VBand     :: Double -> Double -> Plot
  -- | Horizontal band
  HBand     :: Double -> Double -> Plot

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
    -- | Line style
  | LineStyle   String
    -- | Marker style
  | MarkerStyle String
    -- | Fill color (with enum)
  | FillColor  Color
    -- | Histogram options
  | HistOpt    HistOpt
    -- | X axis
  | XAxis Axis
    -- | Y axis
  | YAxis Axis
    -- | Z axis
  | ZAxis Axis

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
data HistOpt
    -- | Draw text
  = HistText Toggle
    -- | Draw colored
  | HistBox Toggle
    -- | Draw colored
  | HistColor Toggle
    -- | Scatter plot
  | HistScatter Toggle
    -- | Contour plot
  | HistContour Int

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
  | Col Int
  deriving (Show,Eq)


-- | Convert command to the string
renderCommand :: Command -> IO String
renderCommand Clear      = return $ "clear"
renderCommand Exit       = return $ "exit"
renderCommand (Save nm)  = do
  -- canonicalizePath doesn't work for nonexistent path
  -- See GHC bugs #4215 #5014
  cwd <- getCurrentDirectory
  rel <- makeRelativeToCurrentDirectory nm
  return $ "save " ++ (show $ cwd ++ "/" ++ rel)
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
renderPlot (Polygon vals) =
  unlines $ ("poly -" : map (\(x,y) -> show x ++ "\t" ++ show y) vals) ++ ["<<<"]
renderPlot (Function    rng f)   =
  renderPlot (FunctionN 128 rng f)
renderPlot (FunctionN n (a,b) f) =
  renderPlot $ Graph [ (x, f x)
                     | i <- [0 .. n]
                     , let x = a + (b - a) * fromIntegral i / fromIntegral n
                     ]
renderPlot (Hist  h   ) =
  unlines [ "hist -"
          , show h ++ "<<<"
          ]
renderPlot (VLine x)   = printf "vline %g" x
renderPlot (HLine x)   = printf "hline %g" x
renderPlot (VBand a b) = printf "vband %g %g" a b
renderPlot (HBand a b) = printf "hband %g %g" a b

-- Option subcommand
renderOption :: Option -> String
renderOption (Silent o)      = "silent " ++ toggle o
renderOption (Title  t)      = "title " ++ show t
renderOption (LineWidth   i) = "line width " ++ show i
renderOption (LineColor   c) = "line color " ++ renderColor c
renderOption (LineStyle   s) = "line style " ++ show s
renderOption (MarkerStyle s) = "line marker " ++ show s
renderOption (FillColor   c) = "fill color " ++ renderColor c
renderOption (HistOpt o )    = "hist " ++ renderHistOpt o
renderOption (XAxis   a )    = "xaxis " ++ renderAxis a
renderOption (YAxis   a )    = "yaxis " ++ renderAxis a
renderOption (ZAxis   a )    = "zaxis " ++ renderAxis a

-- Axis
renderAxis :: Axis -> String
renderAxis (Label str)  = "label " ++ show str
renderAxis  NoLabel     = "label -"
renderAxis (LogScale t) = "log " ++ toggle t
renderAxis (Range a b)  = "range " ++ show a ++ " " ++ show b
renderAxis  RangeAuto   = "range -"

-- Histogram options
renderHistOpt :: HistOpt -> String
renderHistOpt (HistText    o) = "text " ++ toggle o
renderHistOpt (HistBox     o) = "box " ++ toggle o
renderHistOpt (HistColor   o) = "color " ++ toggle o
renderHistOpt (HistScatter o) = "scattter" ++ toggle o
renderHistOpt (HistContour n) = "contour" ++ show n

-- Legend subcommand
renderLegend :: Legend -> String
renderLegend (NewL (x1,y1) (x2,y2)) = printf "add %g %g %g %g" x1 y1 x2 y2
renderLegend DeleteL                = "-"
renderLegend (LegendStr   s)        = "add "       ++ show s
renderLegend (LegendLabel s)        = "add label " ++ show s

toggle :: Toggle -> String
toggle ON  = "on"
toggle OFF = "off"


renderColor :: Color -> String
renderColor c =
  case c of
    WHITE   -> "\"WHITE\""
    BLACK   -> "\"BLACK\""
    RED     -> "\"RED\""
    GREEN   -> "\"GREEN\""
    BLUE    -> "\"BLUE\""
    YELLOW  -> "\"YELLOW\""
    MAGENTA -> "\"MAGENTA\""
    CYAN    -> "\"CYAN\""
    FOREST  -> "\"FOREST\""
    VIOLET  -> "\"VIOLET\""
    Col i   -> show i
