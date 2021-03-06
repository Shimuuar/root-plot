{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
-- | Additional combinators for plotter
module HEP.ROOT.Plot.Extra (
    rectangle
  , irectangle
  , lineRect
  , ilineRect
    -- * Pretty plots
  , addNiceHist
  ) where

import HEP.ROOT.Plot
import qualified Data.Histogram as H


-- | Draw rectangle using polygon command
rectangle :: Real a
          => (a,a)              -- ^ X coordinates
          -> (a,a)              -- ^ Y coordinates
          -> Cmd Command
rectangle (realToFrac -> x1, realToFrac -> x2) (realToFrac -> y1, realToFrac -> y2) = do
  add $ Polygon [ (x1,y1)
                , (x1,y2)
                , (x2,y2)
                , (x2,y1)
                ]

-- | Rectangle for integer plots. It's padded with 0.5 on every side
irectangle :: (Int,Int)         -- ^ X coordinates
           -> (Int,Int)         -- ^ Y coordinates
           -> Cmd Command
irectangle (x1,x2) (y1,y2)
  = rectangle (fromIntegral x1 - 0.5 :: Double, fromIntegral x2 + 0.5)
              (fromIntegral y1 - 0.5 :: Double, fromIntegral y2 + 0.5)


-- | Draw line rectangle using polygon command
lineRect :: Real a
         => (a,a)              -- ^ X coordinates
         -> (a,a)              -- ^ Y coordinates
         -> Cmd Command
lineRect (realToFrac -> x1, realToFrac -> x2) (realToFrac -> y1, realToFrac -> y2) = do
  add $ Graph [ (x1,y1)
              , (x1,y2)
              , (x2,y2)
              , (x2,y1)
              , (x1,y1)
              ]

-- | Rectangle for integer plots. It's padded with 0.5 on every side
ilineRect :: (Int,Int)         -- ^ X coordinates
          -> (Int,Int)         -- ^ Y coordinates
          -> Cmd Command
ilineRect (x1,x2) (y1,y2)
  = lineRect (fromIntegral x1 - 0.5 :: Double, fromIntegral x2 + 0.5)
             (fromIntegral y1 - 0.5 :: Double, fromIntegral y2 + 0.5)


-- | Add nice 1D histogram
addNiceHist :: (Show (H.Histogram bin v)) => H.Histogram bin v -> Cmd Command
addNiceHist h = do
  add $ Hist h
  set $ fillColor (Col 20)
