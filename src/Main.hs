module Main where
  
import System.Directory (getDirectoryContents)
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Foldable (for_)
import Graphics.Image (readImageRGB, writeImage)
import Graphics.Image.Types (VU(VU))

import Quilt (makeQuilt, toImage)
import qualified Quilt

main :: IO ()
main = do

  imgPaths <-
    getDirectoryContents "./img"
    <&> filter (not . startsWith ".")
    <&> fmap ("./img/" <>)

  for_ imgPaths $ putStrLn . ("Found image: " <>)

  putStrLn "Loading images..."
  imgs <- sequence $ readImageRGB VU <$> imgPaths
  putStrLn $ "Loaded " <> show (length imgs) <> " images."

  putStrLn "Quilting..."
  let quilt = makeQuilt 5000 2500 imgs
  putStrLn $ "Used " <> show (length $ Quilt.patches quilt) <> " image(s)"

  --putStrLn "Drawing borders..."
  --withBorders <- Quilt.drawBorders quilt

  putStrLn "Writing to file..."
  writeImage "quilt.png" $ toImage quilt

  putStrLn "All done!"

 where
  startsWith = isPrefixOf
