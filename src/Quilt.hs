module Quilt where
  
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.List (sortOn)
import Graphics.Image (maybeIndex, makeImage, rows, cols)
import Graphics.Image.Interface (fromComponents)

import Shared (Img, Px, firstJust, likeness, border, swap)
import Patch (Patch)
import qualified Patch

data Quilt = Quilt { qdims :: (Int, Int), qpatches :: [Patch] }

qrows :: Quilt -> Int
qrows = fst . qdims

qcols :: Quilt -> Int
qcols = snd . qdims

add :: (Int, Int) -> Img -> Quilt -> Quilt
add (y, x) img quilt = quilt { qpatches = (y, x, img) : qpatches quilt }

overlapsAnything :: Patch -> Quilt -> Bool
overlapsAnything patch quilt = any (overlapping patch) (qpatches quilt)
  where overlapping patch1 patch2 = Patch.bottom patch1 >= Patch.top patch2 && Patch.top patch1 <= Patch.bottom patch2
                                 && Patch.right patch1 >= Patch.left patch2 && Patch.left patch1 <= Patch.right patch2

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


makeQuilt :: Int -> Int -> [Img] -> IO Quilt
makeQuilt height width imgs = placeImgs (sortOn popularity imgs) (Quilt (swap {- <-- TODO FIX -} (height, width)) [])
  where
    popularity this = sum $ imgs <&> \that -> likeness (border this) (border that)

    placeImgs :: [Img] -> Quilt -> IO Quilt
    placeImgs [] quilt = return quilt
    placeImgs (img:restImgs) quilt
      | full quilt = return quilt
      | otherwise = do
        putStrLn "Placing image ..."
        placeImg img quilt & placeImgs restImgs

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

data BorderChunk = BorderChunk OutDir [(Int, Int)]
data OutDir = OutLeft | OutUp | OutRight | OutDown

borderChunks :: Quilt -> [BorderChunk]
borderChunks quilt = qpatches quilt >>= patchBorderChunks
  where
    patchBorderChunks patch@(_oy, _ox, img) =
      [ BorderChunk OutLeft (Patch.leftEdge patch)
      , BorderChunk OutUp (Patch.topEdge patch)
      , BorderChunk OutRight (Patch.rightEdge patch)
      , BorderChunk OutDown (Patch.bottomEdge patch)
      ] & filter (not . borderChunkEnclosed)
      where
        borderChunkEnclosed (BorderChunk _ locs) = locs & all totallyEnclosed
        totallyEnclosed (y, x) = [(y-1, x), (y, x+1), (y+1, x), (y, x-1)]
                                 & all (\neighbor -> maybeIndex img neighbor /= Nothing)
