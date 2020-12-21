module Patch where
  
import Data.Functor ((<&>))
import Graphics.Image (rows, cols)

import Shared (Img)

-- An offset image
data Patch = Patch (Int, Int) Img

left :: Patch -> Int
left (Patch (_, ox) _) = ox

right :: Patch -> Int
right (Patch (_, ox) img) = ox + rows img

top :: Patch -> Int
top (Patch (oy, _) _) = oy

bottom :: Patch -> Int
bottom (Patch (oy, _) img) = oy + cols img

topEdge :: Patch -> [(Int, Int)]
topEdge (Patch (oy, ox) img) = [0 .. cols img - 1] <&> \i -> (oy, ox + i)

rightEdge :: Patch -> [(Int, Int)]
rightEdge (Patch (oy, ox) img) = [0 .. rows img - 1] <&> \i -> (oy + i, ox + cols img)

bottomEdge :: Patch -> [(Int, Int)]
bottomEdge (Patch (oy, ox) img) = [0 .. cols img - 1] <&> \i -> (oy + rows img, ox + i)

leftEdge :: Patch -> [(Int, Int)]
leftEdge (Patch (oy, ox) img) = [0 .. rows img - 1] <&> \i -> (oy + i, ox)
