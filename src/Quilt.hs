{-# LANGUAGE DeriveGeneric #-}

module Quilt where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (sortOn)
import Control.Monad (guard)
import Graphics.Image (maybeIndex, makeImage)
import qualified Graphics.Image as Img
import Graphics.Image.Interface (fromComponents)

import Shared (Img, Px, firstJust, likeness, borderPixels, BorderPoint(..), Border)
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

quiltBorder :: Quilt -> Border
quiltBorder quilt =
  do
    patch <- patches quilt
    borderPoint <- Patch.border patch
    guard $ not . totallyEnclosed $ borderPoint
    guard $ distFromEdge borderPoint > 0
    return borderPoint
  where
    totallyEnclosed (BorderPoint _ (y, x)) = all alreadyInQuilt neighbors
      where
        neighbors = [(y-1, x), (y, x+1), (y+1, x), (y, x-1)]
        alreadyInQuilt point = get point quilt /= Nothing

    distFromEdge (BorderPoint _ (y, x)) = minimum [y, rows quilt - y - 1, x, cols quilt - x - 1]

-- Draws a box around the patches, for debugging purposes
-- Colors: Red=Up, Green=Right, Blue=Down, Yellow=Left
drawBox :: Quilt -> Quilt
drawBox quilt = quilt { patches = borderPatches ++ patches quilt }
  where
    thick = 8
    halfk = thick `div` 2
    borderPatches = patches quilt >>= \(Patch (yOffset, xOffset) img) ->
      [ Patch (yOffset - halfk, xOffset) $ makeImage (thick, Img.cols img) (const $ fromComponents (255, 0, 0))
      , Patch (yOffset, xOffset + Img.cols img - halfk) $ makeImage (Img.rows img, thick) (const $ fromComponents (0, 255, 0))
      , Patch (yOffset + Img.rows img - halfk, xOffset) $ makeImage (thick, Img.cols img) (const $ fromComponents (0, 0, 255))
      , Patch (yOffset - halfk, xOffset) $ makeImage (Img.rows img, thick) (const $ fromComponents (255, 255, 0))
      ]

-- Draws the patches' borders, for debugging purposes
drawBorders :: Quilt -> Quilt
drawBorders quilt = quilt { patches = borderPatches ++ patches quilt }
  where
    thick = 16
    redDot = makeImage (thick, thick) (const $ fromComponents (255, 0, 0))
    borderPatches =
      patches quilt
      >>= Patch.border
      & fmap (\(BorderPoint _ point) -> Patch (point - (thick `div` 2, thick `div` 2)) redDot)
      & everyNth 200

    everyNth :: Int -> [a] -> [a]
    everyNth n xs = xs & mapWithIndex (\i x -> if i `mod` n == 0 then Just x else Nothing) & catMaybes

    mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
    mapWithIndex = mapWithIndexAux 0
      where
        mapWithIndexAux _ _ [] = []
        mapWithIndexAux i f (x:xs) = f i x : mapWithIndexAux (i + 1) f xs

makeQuilt :: Int -> Int -> [Img] -> Quilt
makeQuilt height width imgs = placeImgs (sortOn popularity imgs) (Quilt width height {- <-- TODO: FIX SWAPEDNESS -} [])
  where
    popularity this = sum $ imgs <&> \that -> likeness (borderPixels this) (borderPixels that)

    placeImgs :: [Img] -> Quilt -> Quilt
    placeImgs [] quilt = quilt
    placeImgs (img:restImgs) quilt
      | full quilt = quilt
      | otherwise = placeImg img quilt & placeImgs restImgs

-- Attempt to place a patch onto the quilt, matching borders as well as possible.
-- If unable to fit the whole thing, returns the quilt unchanged.
placeImg :: Img -> Quilt -> Quilt
placeImg img quilt
  -- place in center
  | empty quilt = let oy = (rows quilt - Img.rows img) `div` 2
                      ox = (cols quilt - Img.cols img) `div` 2
                  in add (Patch (oy, ox) img) quilt

  -- attach to outside of existing quilt
  | otherwise =
      do
        anchor <- quiltBorder quilt
        let patch = Patch.attachImage anchor img
        let isOverlapping = patches quilt & any (Patch.overlaps patch)
        guard $ not isOverlapping
        return patch
      & maximumOn adjacencies
      & (\patch -> trace ("Patched: " <> show img) patch)
      & maybe quilt (\patch -> add patch quilt)

  where
    adjacencies patch = length . filter (uncurry adjacent) $ quiltBorder quilt `times` Patch.border patch
    adjacent (BorderPoint _ (y1, x1)) (BorderPoint _ (y2, x2)) = 1 == abs (y2 - y1) + abs (x2 - x1)
    xs `times` ys = xs >>= \x -> ys >>= \y -> return (x, y)

    maximumOn :: Ord k => (v -> k) -> [v] -> Maybe v
    maximumOn _ [] = Nothing
    maximumOn _ [x] = Just x
    maximumOn key (x:y:zs) = maximumOnAux key (maxOn key x y) zs
      where
        maximumOnAux :: Ord k => (v -> k) -> v -> [v] -> Maybe v
        maximumOnAux _ hi [] = Just hi
        maximumOnAux key' hi (a:as) = maximumOnAux key' (maxOn key' a hi) as

        maxOn key' a b = if key' a > key' b then a else b
