module Patch where
  
import Data.Functor ((<&>))
import Graphics.Image (rows, cols)

import Shared (Img)

type Patch = (Int, Int, Img)

left :: Patch -> Int
left (_, ox, _) = ox

right :: Patch -> Int
right (_, ox, img) = ox + rows img

top :: Patch -> Int
top (oy, _, _) = oy

bottom :: Patch -> Int
bottom (oy, _, img) = oy + cols img

topEdge :: Patch -> [(Int, Int)]
topEdge (oy, ox, img) = [0 .. cols img - 1] <&> \i -> (oy, ox + i)

rightEdge :: Patch -> [(Int, Int)]
rightEdge (oy, ox, img) = [0 .. rows img - 1] <&> \i -> (oy + i, ox + cols img)

bottomEdge :: Patch -> [(Int, Int)]
bottomEdge (oy, ox, img) = [0 .. cols img - 1] <&> \i -> (oy + rows img, ox + i)

leftEdge :: Patch -> [(Int, Int)]
leftEdge (oy, ox, img) = [0 .. rows img - 1] <&> \i -> (oy + i, ox)
