module Main where
  
import System.Directory (getDirectoryContents)
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Foldable (for_)
import Graphics.Image (readImageRGB, writeImage)
import Graphics.Image.Types (VU(VU))

import Quilt (makeQuilt, toImage)

main :: IO ()
main = do

  imgPaths <-
    getDirectoryContents "./img"
    <&> filter (not . startsWith ".")
    <&> fmap ("./img/" <>)

  for_ imgPaths $ putStrLn . ("Found image: " <>)

  imgs <- sequence $ readImageRGB VU <$> imgPaths

  let quilt = makeQuilt 10000 5000 imgs
  writeImage "quilt.png" (toImage quilt)

  putStrLn "ok"

 where
  startsWith = isPrefixOf
