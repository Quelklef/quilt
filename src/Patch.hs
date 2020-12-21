{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Patch where
  
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

import Shared (Img)

type Patch = (Int, Int, Img)

left :: Patch -> Int
left (_, ox, _) = ox

right :: Patch -> Int
right (_, ox, img) = ox + rows img

top :: Patch -> Int
top (oy, _, _) = oy

bottom :: Patch -> Int
bottom (oy, _, img) = oy + cols img

topEdge :: Patch -> [(Int, Int)]
topEdge (oy, ox, img) = [0 .. cols img - 1] <&> \i -> (oy, ox + i)

rightEdge :: Patch -> [(Int, Int)]
rightEdge (oy, ox, img) = [0 .. rows img - 1] <&> \i -> (oy + i, ox + cols img)

bottomEdge :: Patch -> [(Int, Int)]
bottomEdge (oy, ox, img) = [0 .. cols img - 1] <&> \i -> (oy + rows img, ox + i)

leftEdge :: Patch -> [(Int, Int)]
leftEdge (oy, ox, img) = [0 .. rows img - 1] <&> \i -> (oy + i, ox)
