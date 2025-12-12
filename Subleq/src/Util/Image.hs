module Util.Image (slqimg) where

import Codec.Picture (DynamicImage, PixelRGB8(..), convertRGB8, pixelAt, imageWidth, imageHeight)
import Data.Word

rgb :: Int -> Int -> Int -> Int
rgb r g b = 0x100*r + 0x10*g + 0x1*b

extract8 :: Word8 -> Int
extract8 x = fromIntegral (div x (2^(4 :: Word8)))

slqPixRGB8 :: PixelRGB8 -> Int
slqPixRGB8 (PixelRGB8 r g b) = rgb
  (extract8 r)
  (extract8 g)
  (extract8 b)

slqimg :: DynamicImage -> [Int]
slqimg di = let
  img = convertRGB8 di
  w = imageWidth img
  h = imageHeight img
  in [ slqPixRGB8 $ pixelAt img x y | y <- [0..h-1], x <- [0..w-1]] 
