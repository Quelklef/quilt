module Quilt where
  
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.List (sortOn)
import Control.Monad (guard)
import Graphics.Image (maybeIndex, makeImage)
import qualified Graphics.Image as Img
import Graphics.Image.Interface (fromComponents)

import Shared (Img, Px, firstJust, likeness, border, borderPixels, BorderPoint(..), safeHead, Border)
import Patch (Patch(..))
import qualified Patch

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
quiltBorder quilt = patches quilt >>= (border . Patch.image)

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
        borderPoint <- quiltBorder quilt
        guard $ not . totallyEnclosed $ borderPoint
        let patch = Patch.fixImageToBorderPoint borderPoint img
        guard $ not . overlapsAnyExistingPatches $ patch
        return $ add patch quilt
      & safeHead
      & fromMaybe quilt

  where
    overlapsAnyExistingPatches :: Patch -> Bool
    overlapsAnyExistingPatches patch = patches quilt & any (Patch.overlaps patch)

    totallyEnclosed :: BorderPoint -> Bool
    totallyEnclosed (BorderPoint _ (y, x)) = neighbors & all alreadyInQuilt
      where
        neighbors = [(y-1, x), (y, x+1), (y+1, x), (y, x-1)]
        alreadyInQuilt point = get point quilt /= Nothing
