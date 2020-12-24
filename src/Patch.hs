{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module Patch where

import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Control.Monad (liftM2)
import qualified Data.List

import Graphics.Image (rows, cols)

import Shared (Img, Anchor(..), AnchorDirection(..), Direction(..))
import qualified Shared

import Debug.Trace

-- An offset image
data Patch = Patch (Int, Int) Img
  deriving (Show)

offset ::  Patch -> (Int, Int)
offset (Patch ost _) = ost

yOffset ::  Patch -> Int
yOffset = fst . offset

xOffset ::  Patch -> Int
xOffset = snd . offset

image :: Patch -> Img
image (Patch _ img) = img

border :: Patch -> [(Int, Int)]
border (Patch offset image) = Shared.border image <&> (+ offset)

side :: Direction -> Patch -> [(Int, Int)]
side direction (Patch offset image) = Shared.side direction image <&> (+ offset)

overlaps :: Patch -> Patch -> Bool
overlaps p1 p2 = bottom p1 >= top p2 && top p1 <= bottom p2
              && right p1 >= left p2 && left p1 <= right p2

-- 'howAdjacent this that' is 'South' if 'this' is adjacent to and above 'that', etc
-- TODO: actually, this technically tests for a looser version than adjacency than I mean
--       since somthing may be 1px above *and* farrrr to the right
howAdjacent :: Patch -> Patch -> Maybe Direction
howAdjacent this that
  | top this == bottom that + 1 && left this <= right that && right this >= left that = Just North
  | right this == left that - 1 && top this <= bottom that && bottom this >= top that = Just East
  | bottom this == top that - 1 && left this <= right that && right this >= left that = Just South
  | left this == right that + 1 && top this <= bottom that && bottom this >= top that = Just West
  | otherwise = Nothing

adjacent :: Patch -> Patch -> Bool
adjacent this that = howAdjacent this that /= Nothing

adjacencies :: Patch -> Patch -> [((Int, Int), (Int, Int))]
adjacencies this that =
  case howAdjacent this that of
    Nothing -> []
    Just direction -> fastSpecialCase & fromMaybe slowerFallback
      where
        thisSide = side direction this
        thatSide = side (Shared.opp direction) that

        thisLength = length thisSide
        thatLength = length thatSide

        -- because we align photos to have a flush edge, then the
        -- most common cases that are:
        --
        -- 1.  AAAAAAAAAAAAAAAAAA
        --     BBBBBBBBBB
        --
        -- 2.  AAAAAAAAAAAAAAAAAA
        --             BBBBBBBBBB
        --
        -- 3.  AAAAAAAAAA
        --     BBBBBBBBBBBBBBBBBB
        --
        -- 4.          AAAAAAAAAA
        --     BBBBBBBBBBBBBBBBBB
        --
        -- Where A represents a point on 'this' photo, and B
        -- represents a point on 'that' photo.
        -- This is only the cases for a vertical adjacency;
        -- it's likewise for a horizontal one.
        fastSpecialCase =
          let
            leftAligned = ptAdjacent (head thisSide) (head thatSide)
            rightAligned = ptAdjacent (last thisSide) (last thatSide)

            takel = take
            taker n xs = drop (length xs - n) xs
          in
            if thisLength >= thatLength
            then if | leftAligned  -> Just $ zip (takel (length thatSide) thisSide) thatSide
                    | rightAligned -> Just $ zip (taker (length thatSide) thisSide) thatSide
                    | otherwise    -> Nothing
            else if | leftAligned  -> Just $ zip thisSide (takel (length thisSide) thatSide)
                    | leftAligned  -> Just $ zip thisSide (taker (length thisSide) thatSide)
                    | otherwise    -> Nothing

        slowerFallback =
          liftM2 (,) (side direction this) (side (Shared.opp direction) that)
          & filter (uncurry ptAdjacent)

        ptAdjacent (y1, x1) (y2, x2) = 1 == abs (y2 - y1) + abs (x2 - x1)

anchors :: Patch -> [Anchor]
anchors patch =
  [ Anchor ToUpRight (top patch - 1, left patch)
  , Anchor ToDownLeft (top patch, left patch - 1)
  , Anchor ToUpLeft (top patch - 1, right patch)
  , Anchor ToDownRight (top patch, right patch + 1)
  , Anchor ToUpRight (bottom patch, right patch + 1)
  , Anchor ToDownLeft (bottom patch + 1, right patch)
  , Anchor ToDownRight (bottom patch + 1, left patch)
  , Anchor ToUpLeft (bottom patch, left patch - 1)
  ]

-- Turns the given image into a patch adjacent to the given border point
attachImage :: Anchor -> Img -> Patch
attachImage (Anchor direction (y, x)) img = Patch patchOffset img
  where patchOffset = case direction of
          ToUpRight -> (y - rows img + 1, x)
          ToUpLeft -> (y - rows img + 1, x - cols img + 1)
          ToDownRight -> (y, x)
          ToDownLeft -> (y, x - cols img + 1)

top :: Patch -> Int
top (Patch (oy, _) _) = oy

right :: Patch -> Int
right (Patch (_, ox) img) = ox + cols img - 1

bottom :: Patch -> Int
bottom (Patch (oy, _) img) = oy + rows img - 1

left :: Patch -> Int
left (Patch (_, ox) _) = ox
