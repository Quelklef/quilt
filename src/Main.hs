module Main where
  
import System.Directory (getDirectoryContents)
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Foldable (for_)
import Graphics.Image (readImageRGB, writeImage)
import Graphics.Image.Types (VU(VU))

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)

import Quilt (makeQuilt, toImage)
import qualified Quilt

-- Evaluate a value strictly
now :: NFData a => a -> IO a
now = evaluate . force

main :: IO ()
main = do

  imgPaths <-
    getDirectoryContents "./img"
    <&> filter (not . startsWith ".")
    <&> fmap ("./img/" <>)

  for_ imgPaths $ putStrLn . ("Found image: " <>)

  putStrLn "Loading images..."
  imgs <- sequence $ readImageRGB VU <$> imgPaths

  putStrLn "Quilting..."
  quilt <- now $ makeQuilt 5000 2500 imgs

  putStrLn $ "Using " <> show (length $ Quilt.patches quilt) <> " image(s)"

  --putStrLn "Drawing borders..."
  --withBorders <- now $ Quilt.drawBorders quilt

  putStrLn "Writing to file..."
  writeImage "quilt.png" $ toImage quilt

  putStrLn "All done!"

 where
  startsWith = isPrefixOf
