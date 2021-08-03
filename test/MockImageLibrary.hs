{-# LANGUAGE StandaloneDeriving #-}

module MockImageLibrary where

import DSL (Orientation (..))
import Data.ByteString.Char8
import Test.QuickCheck

data MockImage = MockImage {width :: Int, height :: Int, orientation :: Orientation, quality :: Int} deriving (Eq, Read, Show)

encode :: MockImage -> ByteString
encode = pack . show

decode :: ByteString -> MockImage
decode = read . unpack

data Direction = CW | CCW deriving (Eq, Show)

rotate :: Direction -> MockImage -> MockImage
rotate d image@(MockImage w h o _) = case d of
  CW -> image {width = h, height = w, orientation = if o == maxBound then minBound else succ o}
  CCW -> image {width = h, height = w, orientation = if o == minBound then maxBound else pred o}

type Width = Int

type Height = Int

resize :: Width -> Height -> MockImage -> MockImage
resize w h image = image {width = w, height = h}

instance Arbitrary Direction where
  arbitrary = elements [CW, CCW]
  shrink CW = [CCW]
  shrink CCW = [CW]

instance Arbitrary Orientation where
  arbitrary = elements [Normal, CW90, UpSideDown, CCW90]
  shrink Normal = [CW90, UpSideDown, CCW90]
  shrink CW90 = [Normal, UpSideDown, CCW90]
  shrink UpSideDown = [Normal, CW90, CCW90]
  shrink CCW90 = [Normal, CW90, UpSideDown]

instance Arbitrary MockImage where
  arbitrary = do
    w <- choose (100, 5000)
    h <- choose (100, 5000)
    o <- arbitrary
    q <- choose (0, 100)
    return
      MockImage
        { width = w,
          height = h,
          orientation = o,
          quality = q
        }
