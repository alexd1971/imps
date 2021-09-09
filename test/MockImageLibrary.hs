{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module MockImageLibrary where

import DSL.ImpLang (Orientation(..), Quality(..), Size)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Test.QuickCheck (Arbitrary(..), choose, elements)

data MockImage =
  MockImage
    { width :: Int
    , height :: Int
    , orientation :: Orientation
    , quality :: Quality
    }
  deriving (Eq, Read, Show)

encode :: MockImage -> ByteString
encode = pack . show

decode :: ByteString -> MockImage
decode = read . unpack

data Direction
  = CW
  | CCW
  deriving (Eq, Show)

rotate :: Direction -> MockImage -> MockImage
rotate d image@(MockImage w h o _) =
  case d of
    CW ->
      image
        { width = h
        , height = w
        , orientation =
            if o == maxBound
              then minBound
              else succ o
        }
    CCW ->
      image
        { width = h
        , height = w
        , orientation =
            if o == minBound
              then maxBound
              else pred o
        }

resize :: Size -> MockImage -> MockImage
resize (w, h) image = image {width = w, height = h}

crop :: Size -> MockImage -> MockImage
crop (w, h) image = image {width = w, height = h}

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
    width <- choose (100, 5000)
    height <- choose (100, 5000)
    orientation <- arbitrary
    q <- choose (-1, 100)
    let quality =
          case q of
            -1 -> Default
            _ -> Quality q
    return MockImage {..}
