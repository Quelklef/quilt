module Quilt where
  
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.List (sortOn)
import Graphics.Image (maybeIndex, makeImage)
import qualified Graphics.Image as Img
import Graphics.Image.Interface (fromComponents)

import Shared (Img, Px, firstJust, likeness, border)
import Patch (Patch(..))
import qualified Patch

data Quilt = Quilt { rows :: Int, cols :: Int, patches :: [Patch] }

dims :: Quilt -> (Int, Int)
dims = (,) <$> rows <*> cols

add :: (Int, Int) -> Img -> Quilt -> Quilt
add (y, x) img quilt = quilt { patches = Patch (y, x) img : patches quilt }

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

get :: Int -> Int -> Quilt -> Maybe Px
get y x quilt = patches quilt <&> (\(Patch (ox, oy) img) -> maybeIndex img (y - oy, x - ox)) & firstJust

get' :: Int -> Int -> Quilt -> Px
get' y x quilt = get y x quilt & fromMaybe (fromComponents (0, 0, 0))

toImage :: Quilt -> Img
toImage quilt = makeImage (dims quilt) (\(y, x) -> get' y x quilt)


makeQuilt :: Int -> Int -> [Img] -> IO Quilt
makeQuilt height width imgs = placeImgs (sortOn popularity imgs) (Quilt width height {- <-- TODO: FIX SWAPEDNESS -} [])
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
  | empty quilt = let oy = (rows quilt - Img.rows img) `div` 2
                      ox = (cols quilt - Img.cols img) `div` 2
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
            let patch = Patch (oy, ox) img
            in if overlapsAnything patch quilt
               then Nothing
               else Just $ add (oy, ox) img quilt)

    calcOffsetGivenBorderChunk chunk (y, x) =
      case chunk of
        BorderChunk OutUp _ -> (y - Img.rows img, x)
        BorderChunk OutRight _ -> (y, x)
        BorderChunk OutDown _ -> (y, x)
        BorderChunk OutLeft _ -> (y - Img.rows img, x - Img.cols img)

data BorderChunk = BorderChunk OutDir [(Int, Int)]
data OutDir = OutLeft | OutUp | OutRight | OutDown

borderChunks :: Quilt -> [BorderChunk]
borderChunks quilt = patches quilt >>= patchBorderChunks
  where
    patchBorderChunks patch@(Patch _ img) =
      [ BorderChunk OutLeft (Patch.leftEdge patch)
      , BorderChunk OutUp (Patch.topEdge patch)
      , BorderChunk OutRight (Patch.rightEdge patch)
      , BorderChunk OutDown (Patch.bottomEdge patch)
      ] & filter (not . borderChunkEnclosed)
      where
        borderChunkEnclosed (BorderChunk _ locs) = locs & all totallyEnclosed
        totallyEnclosed (y, x) = [(y-1, x), (y, x+1), (y+1, x), (y, x-1)]
                                 & all (\neighbor -> maybeIndex img neighbor /= Nothing)
