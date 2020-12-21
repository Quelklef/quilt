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

import Shared (Px, Img, firstJust)
import qualified Patch
import Quilt (makeQuilt, toImage)

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
