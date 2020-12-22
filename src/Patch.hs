{-# LANGUAGE DeriveGeneric #-}

module Patch where

import Data.Functor ((<&>))

import Graphics.Image (rows, cols)

import Shared (Img, Anchor(..), AnchorDirection(..))
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

border :: Patch -> [(Int, Int)]
border patch = Shared.border (image patch) <&> (+ offset patch)

overlaps :: Patch -> Patch -> Bool
overlaps p1 p2 = bottom p1 >= top p2 && top p1 <= bottom p2
              && right p1 >= left p2 && left p1 <= right p2

anchors :: Patch -> [Anchor]
anchors patch =
  [ Anchor ToUpRight (top patch - 1, left patch)
  , Anchor ToDownLeft (top patch, left patch - 1)
  , Anchor ToUpLeft (top patch - 1, right patch)
  , Anchor ToDownRight (top patch, right patch + 1)
  , Anchor ToUpRight (bottom patch, right patch + 1)
  , Anchor ToDownLeft (bottom patch + 1, right patch)
  , Anchor ToDownRight (bottom patch + 1, left patch)
  , Anchor ToUpLeft (bottom patch, left patch - 1)
  ]

-- Turns the given image into a patch adjacent to the given border point
attachImage :: Anchor -> Img -> Patch
attachImage (Anchor direction (y, x)) img = Patch patchOffset img
  where patchOffset = case direction of
          ToUpRight -> (y - rows img + 1, x)
          ToUpLeft -> (y - rows img + 1, x - cols img + 1)
          ToDownRight -> (y, x)
          ToDownLeft -> (y, x - cols img + 1)

top :: Patch -> Int
top (Patch (oy, _) _) = oy

right :: Patch -> Int
right (Patch (_, ox) img) = ox + cols img - 1

bottom :: Patch -> Int
bottom (Patch (oy, _) img) = oy + rows img - 1

left :: Patch -> Int
left (Patch (_, ox) _) = ox
