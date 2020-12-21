module Patch where
  
import Graphics.Image (rows, cols)

import Shared (Img, BorderPoint(..), OutwardsIs(..), Img)

-- An offset image
data Patch = Patch (Int, Int) Img

offset ::  Patch -> (Int, Int)
offset (Patch ost _) = ost

image :: Patch -> Img
image (Patch _ img) = img

overlaps :: Patch -> Patch -> Bool
overlaps p1 p2 = bottom p1 >= top p2 && top p1 <= bottom p2
              || right p1 >= left p2 && left p1 <= right p2

fixImageToBorderPoint :: BorderPoint -> Img -> Patch
fixImageToBorderPoint (BorderPoint outwardsIs (y, x)) img =
  case outwardsIs of
    OutwardsIsUp -> Patch (y - rows img, x) img
    OutwardsIsRight -> Patch (y, x) img
    OutwardsIsDown -> Patch (y, x) img
    OutwardsIsLeft -> Patch (y - rows img, x - cols img) img

left :: Patch -> Int
left (Patch (_, ox) _) = ox

right :: Patch -> Int
right (Patch (_, ox) img) = ox + rows img

top :: Patch -> Int
top (Patch (oy, _) _) = oy

bottom :: Patch -> Int
bottom (Patch (oy, _) img) = oy + cols img
