{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Quilt where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad (guard)
import Graphics.Image (maybeIndex, makeImage)
import qualified Graphics.Image as Img
import Graphics.Image.Interface (fromComponents)

import Shared (Img, Px, firstJust, Anchor(..), pixelLikeness, maximumOn)
import Patch (Patch(..))
import qualified Patch

import Debug.Trace

data Quilt = Quilt { rows :: Int, cols :: Int, patches :: [Patch] }

dims :: Quilt -> (Int, Int)
dims = (,) <$> rows <*> cols

add :: Patch -> Quilt -> Quilt
add patch quilt = quilt { patches = patch : patches quilt }

overlapsAnything :: Patch -> Quilt -> Bool
overlapsAnything patch quilt = any (overlapping patch) (patches quilt)
  where overlapping patch1 patch2 = Patch.bottom patch1 >= Patch.top patch2 && Patch.top patch1 <= Patch.bottom patch2
                                 && Patch.right patch1 >= Patch.left patch2 && Patch.left patch1 <= Patch.right patch2

area :: Quilt -> Int
area quilt = uncurry (*) (dims quilt)

takenSpace :: Quilt -> Int
takenSpace quilt = sum $ spaceTaken <$> patches quilt
  where spaceTaken (Patch (ox, oy) img) =
            (Img.rows img - (max 0 $ negate ox)
                          - (max 0 $ ox + Img.rows img - rows quilt))
          + (Img.cols img - (max 0 $ negate oy)
                          - (max 0 $ oy + Img.cols img - cols quilt))

freeSpace :: Quilt -> Int
freeSpace quilt = area quilt - takenSpace quilt

full :: Quilt -> Bool
full = (0 ==) . freeSpace

empty :: Quilt -> Bool
empty = null . patches

get :: (Int, Int) -> Quilt -> Maybe Px
get point quilt = patches quilt <&> (\(Patch offset img) -> maybeIndex img (point - offset)) & firstJust

get' :: (Int, Int) -> Quilt -> Px
get' point quilt = get point quilt & fromMaybe (fromComponents (0, 0, 0))

toImage :: Quilt -> Img
toImage quilt = makeImage (dims quilt) (\point -> get' point quilt)

anchors :: Quilt -> [Anchor]
anchors quilt = patches quilt >>= Patch.anchors & filter (not . totallyEnclosed)
  where
    totallyEnclosed (Anchor _ point) = all alreadyInQuilt (neighbors point)
    neighbors (y, x) = [(y-1, x), (y, x+1), (y+1, x), (y, x-1)]
    alreadyInQuilt point = get point quilt /= Nothing

-- Draws the patches' borders, for debugging purposes
drawAnchors :: Quilt -> Quilt
drawAnchors quilt = quilt { patches = anchorPatches ++ patches quilt }
  where
    thick = 4
    redDot = makeImage (thick, thick) (const $ fromComponents (1.0, 0, 0))
    anchorPatches =
      patches quilt
      >>= Patch.anchors
      & fmap (\(Anchor _ point) -> Patch (point - (thick `div` 2, thick `div` 2)) redDot)

-- Attempt to place a patch onto the quilt, matching borders as well as possible.
-- If unable to fit the whole thing, returns Nothing
placeImg :: Img -> Quilt -> Maybe Quilt
placeImg img quilt
  -- place in center
  | empty quilt = let oy = (rows quilt - Img.rows img) `div` 2
                      ox = (cols quilt - Img.cols img) `div` 2
                  in Just $ add (Patch (oy, ox) img) quilt

  -- attach to outside of existing quilt
  | otherwise =
      do
        return () & trace ((show . length $ anchors quilt) <> " anchors")
        anchor <- anchors quilt
        guard $ inBounds anchor
        let patch = Patch.attachImage anchor img
        let isOverlapping = patches quilt & any (Patch.overlaps patch)
        guard $ not isOverlapping
        return patch
      & maximumOn goodness
      <&> (\patch -> trace ("Patched: " <> show img) patch)
      <$> (\patch -> add patch quilt)

  where
    inBounds (Anchor _ (y, x)) = y >= 0 && y < cols quilt && x >= 0 && x < rows quilt

    goodness :: Patch -> Double
    goodness patch =
      let quiltWithPatch = add patch quilt in
      adjacencies patch
      & fmap (\(pt1, pt2) -> fromJust $ pixelLikeness <$> get pt1 quiltWithPatch <*> get pt2 quiltWithPatch)
      & sum

    adjacencies :: Patch -> [((Int, Int), (Int, Int))]
    adjacencies patch = patches quilt >>= patchAdjacencies
      where patchAdjacencies otherPatch =
              if not (Patch.adjacent patch otherPatch)
              then []
              else Patch.border patch `times` Patch.border otherPatch
                   & filter (uncurry adjacent)

    adjacent (y1, x1) (y2, x2) = 1 == abs (y2 - y1) + abs (x2 - x1)
    xs `times` ys = xs >>= \x -> ys >>= \y -> return (x, y)
