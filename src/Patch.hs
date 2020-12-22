{-# LANGUAGE DeriveGeneric #-}

module Patch where

import Graphics.Image (rows, cols)

import Data.Functor ((<&>))

import Shared (Img, BorderPoint(..), OutwardsIs(..), Img, Border, BorderPoint(..))
import qualified Shared

-- An offset image
data Patch = Patch (Int, Int) Img

offset ::  Patch -> (Int, Int)
offset (Patch ost _) = ost

yOffset ::  Patch -> Int
yOffset = fst . offset

xOffset ::  Patch -> Int
xOffset = snd . offset

image :: Patch -> Img
image (Patch _ img) = img

border :: Patch -> Border
border patch = Shared.border (image patch) <&> (\(BorderPoint outwardsIs point) -> BorderPoint outwardsIs (offset patch + point))

overlaps :: Patch -> Patch -> Bool
overlaps p1 p2 = bottom p1 >= top p2 && top p1 <= bottom p2
              && right p1 >= left p2 && left p1 <= right p2

-- Turns the given image into a patch adjacent to the given border point
attachImage :: BorderPoint -> Img -> Patch
attachImage (BorderPoint outwardsIs (y, x)) img =
  case outwardsIs of
      OutwardsIsUp    -> Patch (y - rows img, x) img
      OutwardsIsRight -> Patch (y, x + 1) img
      OutwardsIsDown  -> Patch (y + 1, x) img
      OutwardsIsLeft  -> Patch (y - rows img, x - cols img) img

top :: Patch -> Int
top (Patch (oy, _) _) = oy

right :: Patch -> Int
right (Patch (_, ox) img) = ox + cols img - 1

bottom :: Patch -> Int
bottom (Patch (oy, _) img) = oy + rows img - 1

left :: Patch -> Int
left (Patch (_, ox) _) = ox
