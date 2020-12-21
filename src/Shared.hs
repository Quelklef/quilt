{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}

module Shared where

import Data.Function (on)
import Data.Functor ((<&>))
import qualified Data.MultiSet as MultiSet
import Data.MultiSet (MultiSet)
import Graphics.Image (toLists, dims, index)
import Graphics.Image.Interface (toComponents)
import Graphics.Image.Types (Image, RGB, VU(..), Pixel)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing : xs) = firstJust xs
firstJust (Just x : _) = Just x

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

type Px = Pixel RGB Double
type Img = Image VU RGB Double

-- Ord instances needed for MultiSet usage
instance Ord Px where
  compare = compare `on` toComponents
instance Ord Img where
  compare = compare `on` toLists

likeness :: MultiSet Px -> MultiSet Px -> Double
likeness pxs1 pxs2 = negate dissimilarity
  where
    dissimilarity = sum $ MultiSet.map (distanceToClosest pxs2) pxs1
    distanceToClosest pxs px = minimum $ MultiSet.map (pixelDissimilarity px) pxs
    pixelDissimilarity :: Px -> Px -> Double
    pixelDissimilarity px1 px2 =
      let (r1, g1, b1) = toComponents px1
          (r2, g2, b2) = toComponents px2
      in abs (r1 - r2) + abs (g1 - g2) + abs (b1 - b2)

-- Border of an image, starting top-left and proceeding counter-clockwise
border :: Img -> MultiSet Px
border img =
  let (height, width) = dims img
  in mconcat $ [0 .. 2 * (height + width) - 4 - 1] <&> \idx ->
    let (y, x) = if | idx < 1*height + 0*width - 0 -> (idx + 0, 0)
                    | idx < 1*height + 1*width - 1 -> (height - 1, idx - height + 1)
                    | idx < 2*height + 1*width - 2 -> (height - (idx - width - height + 2), width - 1)
                    | idx < 2*height + 2*width - 3 -> (0, width - (idx - height - width - height + 3))
                    | otherwise -> error "Broken code"
    in MultiSet.singleton $ index img (y, x)
