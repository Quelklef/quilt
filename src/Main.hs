{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where
  
import System.Directory (getDirectoryContents)
import Control.Monad (join)
import Data.Function ((&), on)
import Data.Functor ((<&>))
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (isPrefixOf, (\\), sortBy, sortOn)
import qualified Data.MultiSet as MultiSet
import Data.MultiSet (MultiSet)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Traversable (for)
import Graphics.Image (readImageRGB, writeImage, toLists, maybeIndex, dims, index, makeImage, rows, cols)
import Graphics.Image.Interface (toComponents, fromComponents)
import Graphics.Image.Types (Image, RGB, VU(..), Pixel)

firstJust [] = Nothing
firstJust (Nothing : xs) = firstJust xs
firstJust (Just x : _) = Just x

type Px = Pixel RGB Double
type Img = Image VU RGB Double

-- Ord instances needed for MultiSet usage
instance Ord Px where
  compare = compare `on` toComponents
instance Ord Img where
  compare = compare `on` toLists

topEdge :: Patch -> [(Int, Int)]
topEdge (oy, ox, img) = [0 .. cols img - 1] <&> \i -> (oy, ox + i)

rightEdge :: Patch -> [(Int, Int)]
rightEdge (oy, ox, img) = [0 .. rows img - 1] <&> \i -> (oy + i, ox + cols img)

bottomEdge :: Patch -> [(Int, Int)]
bottomEdge (oy, ox, img) = [0 .. cols img - 1] <&> \i -> (oy + rows img, ox + i)

leftEdge :: Patch -> [(Int, Int)]
leftEdge (oy, ox, img) = [0 .. rows img - 1] <&> \i -> (oy + i, ox)

likeness :: MultiSet Px -> MultiSet Px -> Double
likeness pxs1 pxs2 = likeness
  where
    likeness = negate dissimilarity
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

data BorderChunk = BorderChunk OutDir [(Int, Int)]
data OutDir = OutLeft | OutUp | OutRight | OutDown

borderChunks :: Quilt -> [BorderChunk]
borderChunks quilt = qpatches quilt >>= patchBorderChunks
  where
    patchBorderChunks patch@(oy, ox, img) =
      [ BorderChunk OutLeft (leftEdge patch)
      , BorderChunk OutUp (topEdge patch)
      , BorderChunk OutRight (rightEdge patch)
      , BorderChunk OutDown (bottomEdge patch)
      ] & filter (not . borderChunkEnclosed)
      where
        borderChunkEnclosed (BorderChunk _ locs) = locs & all totallyEnclosed
        totallyEnclosed (y, x) = [(y-1, x), (y, x+1), (y+1, x), (y, x-1)]
                                 & all (\neighbor -> maybeIndex img neighbor /= Nothing)

-- A quilt is a canvas populated by a collection of offset images
-- The assumption is that the images do not overlap
type Patch = (Int, Int, Img)
data Quilt = Quilt { qdims :: (Int, Int), qpatches :: [Patch] }

qrows :: Quilt -> Int
qrows = fst . qdims

qcols :: Quilt -> Int
qcols = snd . qdims

add :: (Int, Int) -> Img -> Quilt -> Quilt
add (y, x) img quilt = quilt { qpatches = (y, x, img) : qpatches quilt }

patchLeft :: Patch -> Int
patchLeft (_, ox, _) = ox

patchRight :: Patch -> Int
patchRight (_, ox, img) = ox + rows img

patchTop :: Patch -> Int
patchTop (oy, _, _) = oy

patchBottom :: Patch -> Int
patchBottom (oy, _, img) = oy + cols img

overlapsAnything :: Patch -> Quilt -> Bool
overlapsAnything patch quilt = any (overlapping patch) (qpatches quilt)
  where overlapping patch1 patch2 = patchBottom patch1 >= patchTop patch2 && patchTop patch1 <= patchBottom patch2
                                 && patchRight patch1 >= patchLeft patch2 && patchLeft patch1 <= patchRight patch2

area :: Quilt -> Int
area quilt = uncurry (*) (qdims quilt)

takenSpace :: Quilt -> Int
takenSpace quilt = sum $ spaceTaken <$> qpatches quilt
  where spaceTaken (ox, oy, img) = (rows img - (max 0 $ negate ox)
                                             - (max 0 $ ox + rows img - qrows quilt))
                                 + (cols img - (max 0 $ negate oy)
                                             - (max 0 $ oy + cols img - qcols quilt))

freeSpace :: Quilt -> Int
freeSpace quilt = area quilt - takenSpace quilt

full :: Quilt -> Bool
full = (0 ==) . freeSpace

empty :: Quilt -> Bool
empty = null . qpatches

get :: Int -> Int -> Quilt -> Maybe Px
get y x quilt = qpatches quilt <&> (\(ox, oy, img) -> maybeIndex img (y - oy, x - ox)) & firstJust

get' :: Int -> Int -> Quilt -> Px
get' y x quilt = get y x quilt & fromMaybe (fromComponents (0, 0, 0))

toImage :: Quilt -> Img
toImage quilt = makeImage (qdims quilt) (\(y, x) -> get' y x quilt)

swap (a, b) = (b, a)

makeQuilt :: Int -> Int -> [Img] -> IO Quilt
makeQuilt height width imgs = placeImgs (sortOn popularity imgs) (Quilt (swap {- <-- TODO FIX -} (height, width)) [])
  where
    popularity this = sum $ imgs <&> \that -> likeness (border this) (border that)

    placeImgs :: [Img] -> Quilt -> IO Quilt
    placeImgs [] quilt = return quilt
    placeImgs (img:imgs) quilt
      | full quilt = return quilt
      | otherwise = do
        putStrLn "Placing image ..."
        placeImg img quilt & placeImgs imgs

-- Attempt to place a patch onto the quilt, matching borders as well as possible.
-- If unable to fit the whole thing, returns the quilt unchanged.
placeImg :: Img -> Quilt -> Quilt
placeImg img quilt
  -- place in center
  | empty quilt = let oy = (qrows quilt - rows img) `div` 2
                      ox = (qcols quilt - cols img) `div` 2
                  in add (oy, ox) img quilt
  -- attach to outside of existing quilt
  | otherwise =
      (borderChunks quilt <&> \chunk -> placeAlong chunk) & firstJust & fromMaybe quilt
  where
    placeAlong :: BorderChunk -> Maybe Quilt
    placeAlong chunk@(BorderChunk _ locs) =
      firstJust $ locs <&> (\loc ->
        case calcOffsetGivenBorderChunk chunk loc of
          (oy, ox) ->
            let patch = (oy, ox, img)
            in if overlapsAnything patch quilt
               then Nothing
               else Just $ add (oy, ox) img quilt)

    calcOffsetGivenBorderChunk chunk (y, x) =
      case chunk of
        BorderChunk OutUp _ -> (y - rows img, x)
        BorderChunk OutRight _ -> (y, x)
        BorderChunk OutDown _ -> (y, x)
        BorderChunk OutLeft _ -> (y - rows img, x - cols img)


main :: IO ()
main = do

  imgPaths <-
    getDirectoryContents "./img"
    <&> filter (not . startsWith ".")
    <&> fmap ("./img/" <>)

  for imgPaths $ putStrLn . ("Found image: " <>)

  imgs <- sequence $ readImageRGB VU <$> imgPaths

  quilt <- makeQuilt 10000 5000 imgs
  writeImage "quilt.png" (toImage quilt)

  putStrLn "ok"

 where
  startsWith = isPrefixOf
