{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}

module Shared where

import Data.Function (on)
import Data.Functor ((<&>))
import Data.Maybe (listToMaybe)
import Graphics.Image (toLists, dims)
import Graphics.Image.Interface (toComponents)
import Graphics.Image.Types (Image, RGB, VS, Pixel)
import Graphics.Image.Interface (Elevator)
import Foreign.Storable (Storable)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing : xs) = firstJust xs
firstJust (Just x : _) = Just x

safeHead :: [a] -> Maybe a
safeHead = listToMaybe

-- Complex numbers as 2-tuples, for convenience
instance Num n => Num (n, n) where
  (a, b) + (c, d) = (a + c, b + d)
  (a, b) - (c, d) = (a - c, b - d)
  (a, b) * (c, d) = (a * c - b * d, a * d + b * c)
  negate (a, b) = (negate a, negate b)
  abs (a, b) = (abs a, abs b)
  signum _ = undefined  -- not used, so elide for now
  fromInteger n = (fromInteger n, 0)

type Px = Pixel RGB Double
type Img = Image VS RGB Double

-- Ord instances needed for MultiSet usage
instance (Storable e, Elevator e, Ord e) => Ord (Pixel RGB e) where
  compare = compare `on` toComponents
instance (Storable e, Elevator e, Ord e) => Ord (Image VS RGB e)  where
  compare = compare `on` toLists

data Anchor = Anchor AnchorDirection (Int, Int)
data AnchorDirection = ToDownRight | ToDownLeft | ToUpLeft | ToUpRight

pixelLikeness :: Px -> Px -> Double
pixelLikeness px1 px2 =
  let (r1, g1, b1) = toComponents px1
      (r2, g2, b2) = toComponents px2
  in 1 / (abs (r1 - r2) + abs (g1 - g2) + abs (b1 - b2))

-- Border of an image, starting top-left and proceeding clockwise
border :: Img -> [(Int, Int)]
border img = reverse $ [0 .. 2 * (height + width) - 4 - 1] <&> mkBorderPoint
  where
    (height, width) = dims img
    mkBorderPoint idx
      | idx < 1*height + 0*width - 0 = (idx + 0, 0)
      | idx < 1*height + 1*width - 1 = (height - 1, idx - height + 1)
      | idx < 2*height + 1*width - 2 = (height - (idx - width - height + 2), width - 1)
      | idx < 2*height + 2*width - 3 = (0, width - (idx - height - width - height + 3))
      | otherwise = error "Broken code"

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = map (uncurry f) $ zip [0..] xs

maximumOn :: Ord k => (v -> k) -> [v] -> Maybe v
maximumOn _ [] = Nothing
maximumOn _ [x] = Just x
maximumOn key (x:y:zs) = maximumOnAux key (maxOn key x y) zs
  where
    maximumOnAux :: Ord k => (v -> k) -> v -> [v] -> Maybe v
    maximumOnAux _ hi [] = Just hi
    maximumOnAux key' hi (a:as) = maximumOnAux key' (maxOn key' a hi) as

    maxOn key' a b = if key' a > key' b then a else b
