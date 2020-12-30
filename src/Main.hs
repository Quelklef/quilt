{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Directory (getDirectoryContents)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Graphics.Image.Types (VS)
import Graphics.Image (Image, RGB, readImage, writeImage, index)
import qualified Graphics.Image as Img
import Graphics.Image.Interface (fromComponents, toComponents)
import Graphics.Image.Processing (scale, Border(Edge))

import Shared (Img, Px, mapWithIndex)
import qualified Shared
import Quilt (Quilt(..), toImage)
import qualified Quilt

main :: IO ()
main = do

  imgPaths <-
    getDirectoryContents "./img"
    <&> filter (not . startsWith ".")
    <&> fmap ("./img/" <>)

  let imgCount = length imgPaths

  putStrLn $ "Found " <> show imgCount <> " images"

  putStrLn "Creating border table..."
  table <- imgPaths
           & mapWithIndex (\idx path -> do
             putStrLn $ "Processing #" <> show (idx + 1) <> "/" <> show imgCount <> "..."
             eImg <- readImage path :: IO (Either String (Image VS RGB Double))
             case eImg of
               Left err -> do { putStrLn $ "Failed to read: " <> err ; return Nothing }
               Right img -> do
                 let border = characterizeBorder img
                 putStrLn . show $ border  -- This line fixes a memory leak lol
                 return $ Just (path, border))
           & sequence
           <&> catMaybes
           <&> Map.fromList

  putStrLn "Quilting..."
  quilt <- makeQuilt (1920, 1080) table
  putStrLn $ "Using " <> show (length $ Quilt.patches quilt) <> " image(s)"

  --putStrLn "Drawing anchors..."
  --let withAnchors = Quilt.drawAnchors quilt

  putStrLn "Writing to file..."
  writeImage "quilt.png" $ toImage quilt

  putStrLn "All done!"

 where
  startsWith = isPrefixOf

  makeQuilt :: (Int, Int) -> Map String (MultiSet Thixel) -> IO Quilt
  makeQuilt (quiltHeight, quiltWidth) table =
      placeImgs (sortOn popularity (Map.keys table))
                (Quilt quiltWidth quiltHeight [] {- <-- TODO: FIX WIDTH/HEIGHT SWAPEDNESS -})
    where
      popularity this = sum $ Map.keys table <&> \that -> likeness (lookupM this table) (lookupM that table)

      placeImgs :: [FilePath] -> Quilt -> IO Quilt
      placeImgs [] quilt = return quilt
      placeImgs (path:paths) quilt = do
        putStrLn $ "Sewing; " <> show (length paths) <> " left (now: " <> path <> ")"
        eImg <- readImage path :: IO (Either String (Image VS RGB Double))
        case eImg of
          Left err -> do { putStrLn $ "Failed to read image " <> path <> ": " <> err; placeImgs paths quilt }
          Right img ->
            let mPlaced = Quilt.placeImg img quilt
            in case mPlaced of
              Nothing -> return quilt
              Just placed -> placeImgs paths placed

  lookupM :: (Ord k, Monoid v) => k -> Map k v -> v
  lookupM k m = Map.lookup k m & fromMaybe mempty


newtype Thixel = Thixel { unThixel :: (Bucket, Bucket, Bucket) }
  deriving (Ord, Eq, Show)

data Bucket = VeryLow | Low | KindaLow | Medium | KindaHigh | High | VeryHigh
  deriving (Ord, Eq, Bounded, Show)

toBucket :: Double -> Bucket
toBucket d
  | d <= 1/7 = VeryLow
  | d <= 2/7 = Low
  | d <= 3/7 = KindaLow
  | d <= 4/7 = Medium
  | d <= 5/7 = KindaHigh
  | d <= 6/7 = High
  | d <= 7/7 = VeryHigh
  | otherwise = VeryHigh

fromBucket :: Bucket -> Double
fromBucket VeryLow   = 0.5/7
fromBucket Low       = 1.5/7
fromBucket KindaLow  = 2.5/7
fromBucket Medium    = 3.5/7
fromBucket VeryHigh  = 4.5/7
fromBucket High      = 5.5/7
fromBucket KindaHigh = 6.5/7

toThixel :: Px -> Thixel
toThixel = Thixel . map3 toBucket . toComponents

fromThixel :: Thixel -> Px
fromThixel = fromComponents . map3 fromBucket . unThixel

map3 :: (a -> b) -> (a, a, a) -> (b, b, b)
map3 f (a, b, c) = (f a, f b, f c)

characterizeBorder :: Img -> MultiSet Thixel
characterizeBorder img = Shared.border img <&> index img <&> toThixel <&> MultiSet.singleton & mconcat

likeness :: MultiSet Thixel -> MultiSet Thixel -> Double
likeness pxs1 pxs2 = sum $ MultiSet.map (highestLikenessIn $ MultiSet.map fromThixel pxs2) pxs1
  where highestLikenessIn pxs px = maximum $ MultiSet.map (Shared.pixelLikeness $ fromThixel px) pxs
