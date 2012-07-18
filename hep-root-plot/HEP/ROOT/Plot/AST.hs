{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Monoid
import System.Directory       (getCurrentDirectory,makeRelativeToCurrentDirectory)
import Text.Printf
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import qualified Data.ByteString  as BS
import qualified Data.ByteString.Char8 ()


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
  -- Simple graph
  Graph     :: [(Double,Double)] -> Plot
  -- Sequence of points
  Graph1    :: [Double]          -> Plot
  -- Polygon
  Polygon   :: [(Double,Double)] -> Plot
  -- Plot function
  Function  :: (Double,Double) -> (Double -> Double) -> Plot
  -- Plot function using many points at same time
  FunctionN :: Int -> (Double,Double) -> (Double -> Double) -> Plot
  -- Plot histogram
  Hist      :: Show (Histogram v bin a) => Histogram v bin a -> Plot
  -- Vertical line
  VLine     :: Double -> Plot
  -- Horizontal line
  HLine     :: Double -> Plot
  -- Vertical band
  VBand     :: Double -> Double -> Plot
  -- Horizontal band
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
    -- | Axis label
  = Label String
    -- | No label
  | NoLabel
    -- | Set log scale
  | LogScale Toggle
    -- | Set range
  | Range Double Double
    -- | Set automatic range
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
    -- | Palette for color plot
  | HistPalette Toggle

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
renderCommand :: Command -> IO Builder
renderCommand Clear      = return $ co "clear\n"
renderCommand Exit       = return $ co "exit\n"
renderCommand (Save nm)  = do
  -- canonicalizePath doesn't work for nonexistent path
  -- See GHC bugs #4215 #5014
  cwd <- getCurrentDirectory
  rel <- makeRelativeToCurrentDirectory nm
  return $ co "save " <> (strLit $ cwd ++ "/" ++ rel) <> co "\n"
renderCommand (Set opt)  = return $ co "set  "   <> renderOption opt <> co "\n"
renderCommand (Plot pl)  = return $ co "plot "   <> renderPlot pl    <> co "\n"
renderCommand (Add  pl)  = return $ co "add  "   <> renderPlot pl    <> co "\n"
renderCommand (Legend l) = return $ co "legend " <> renderLegend l   <> co "\n"

-- plot subcommand
renderPlot :: Plot -> Builder
renderPlot (Graph vals) =
  fromString $ unlines $ ("graph -" : map (\(x,y) -> show x ++ "\t" ++ show y) vals) ++ ["<<<"]
renderPlot (Graph1 ys ) =
  fromString $ unlines $ ("graph -" : map show ys) ++ ["<<<"]
renderPlot (Polygon vals) =
  fromString $ unlines $ ("poly -" : map (\(x,y) -> show x ++ "\t" ++ show y) vals) ++ ["<<<"]
renderPlot (Function    rng f)   =
  renderPlot (FunctionN 128 rng f)
renderPlot (FunctionN n (a,b) f) =
  renderPlot $ Graph [ (x, f x)
                     | i <- [0 .. n]
                     , let x = a + (b - a) * fromIntegral i / fromIntegral n
                     ]
renderPlot (Hist  h   ) 
  =  co "hist -\n"
  <> fromString (show h)
  <> co "<<<\n"
renderPlot (VLine x)   = co "vline " <> real x <> co "\n"
renderPlot (HLine x)   = co "hline " <> real x <> co "\n"
renderPlot (VBand a b) = co "vband " <> real a <> co " " <> real b <> co "\n"
renderPlot (HBand a b) = co "hband " <> real a <> co " " <> real b <> co "\n"

-- Option subcommand
renderOption :: Option -> Builder
renderOption (Silent o)      = co "silent "      <> toggle o
renderOption (Title  t)      = co "title "       <> strLit t
renderOption (LineWidth   i) = co "line width "  <> int i
renderOption (LineColor   c) = co "line color "  <> renderColor c
renderOption (LineStyle   s) = co "line style "  <> strLit s
renderOption (MarkerStyle s) = co "line marker " <> strLit s
renderOption (FillColor   c) = co "fill color "  <> renderColor c
renderOption (HistOpt o )    = co "hist "  <> renderHistOpt o
renderOption (XAxis   a )    = co "xaxis " <> renderAxis a
renderOption (YAxis   a )    = co "yaxis " <> renderAxis a
renderOption (ZAxis   a )    = co "zaxis " <> renderAxis a

-- Axis
renderAxis :: Axis -> Builder
renderAxis (Label str)  = co "label " <> strLit str
renderAxis  NoLabel     = co "label -"
renderAxis (LogScale t) = co "log "   <> toggle t
renderAxis (Range a b)  = co "range " <> real a <> co " " <> real b
renderAxis  RangeAuto   = co "range -"

-- Histogram options
renderHistOpt :: HistOpt -> Builder
renderHistOpt (HistText    o) = co "text "    <> toggle o
renderHistOpt (HistBox     o) = co "box "     <> toggle o
renderHistOpt (HistColor   o) = co "color "   <> toggle o
renderHistOpt (HistScatter o) = co "scattter" <> toggle o
renderHistOpt (HistContour n) = co "contour"  <> int n
renderHistOpt (HistPalette p) = co "palette " <> toggle p

-- Legend subcommand
renderLegend :: Legend -> Builder
renderLegend (NewL (x1,y1) (x2,y2)) = fromString $ printf "add %g %g %g %g" x1 y1 x2 y2
renderLegend DeleteL                = co "-"
renderLegend (LegendStr   s)        = co "add "       <> strLit s
renderLegend (LegendLabel s)        = co "add label " <> strLit s

toggle :: Toggle -> Builder
toggle ON  = copyByteString "on"
toggle OFF = copyByteString "off"


renderColor :: Color -> Builder
renderColor c =
  case c of
    WHITE   -> copyByteString "\"WHITE\""
    BLACK   -> copyByteString "\"BLACK\""
    RED     -> copyByteString "\"RED\""
    GREEN   -> copyByteString "\"GREEN\""
    BLUE    -> copyByteString "\"BLUE\""
    YELLOW  -> copyByteString "\"YELLOW\""
    MAGENTA -> copyByteString "\"MAGENTA\""
    CYAN    -> copyByteString "\"CYAN\""
    FOREST  -> copyByteString "\"FOREST\""
    VIOLET  -> copyByteString "\"VIOLET\""
    Col i   -> fromShow i


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

co :: BS.ByteString -> Builder
co = copyByteString

int :: Int -> Builder
int = fromShow

real :: Double -> Builder
real = fromShow

strLit :: String -> Builder
strLit = fromShow

class HistShow h where
  histShow :: h -> BS.ByteString

-- instance (Show a, Show (BinValue bin), Show bin, Bin bin, Vector v a) => Show (Histogram v bin a) where
--     show h@(Histogram bin uo _) = "# Histogram\n" ++ showUO uo ++ show bin ++
--                                   unlines (fmap showT $ asList h)
--         where
--           showT (x,y) = show x ++ "\t" ++ show y
--           showUO (Just (u,o)) = "# Underflows = " ++ show u ++ "\n" ++
--                                 "# Overflows  = " ++ show o ++ "\n"
--           showUO Nothing      = "# Underflows = \n" ++
--                                 "# Overflows  = \n"
