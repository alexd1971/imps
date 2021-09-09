{-# LANGUAGE RecordWildCards #-}

module Tests.Helpers.ResizeRules
  ( testResizeRules
  ) where

import DSL.ImpLang
import Helpers.ResizeRules
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

data TestData =
  TestData
    { mw :: Maybe Width
    , mh :: Maybe Height
    , w' :: Width
    , h' :: Height
    }
  deriving (Show)

testData :: Gen TestData
testData = do
  w <- arbitrary :: Gen Int
  h <- arbitrary :: Gen Int
  w' <- choose (100, 5000)
  h' <- choose (100, 5000)
  let mw =
        case w < 100 of
          True -> Nothing
          _ -> Just w
      mh =
        case h < 100 of
          True -> Nothing
          _ -> Just h
  return TestData {..}

containRuleProp :: TestData -> Bool
containRuleProp TestData {..} =
  case (mw, mh) of
    (Nothing, Nothing) ->
      let size'' = containRule mw mh (w', h')
       in size'' == (w', h')
    (Just w, Nothing) ->
      let (w'', h'') = containRule (Just w) mh (w', h')
       in w'' == w && abs (ratio h' w' - ratio h'' w'') < 1
    (Nothing, Just h) ->
      let (w'', h'') = containRule mw (Just h) (w', h')
       in h'' == h && abs (ratio h'' w'' - ratio h' w') < 1
    (Just w, Just h) ->
      let (w'', h'') = containRule (Just w) (Just h) (w', h')
       in w'' == w && h'' <= h || h'' == h && w'' <= w

coverRuleProp :: TestData -> Bool
coverRuleProp TestData {..} =
  case (mw, mh) of
    (Nothing, Nothing) ->
      let size'' = containRule mw mh (w', h')
      in size'' == (w', h')
    (Just w, Nothing) ->
      let (w'', h'') = containRule (Just w) mh (w', h')
      in w'' == w && abs (ratio h' w' - ratio h'' w'') < 1
    (Nothing, Just h) ->
      let (w'', h'') = containRule mw (Just h) (w', h')
      in h'' == h && abs (ratio h'' w'' - ratio h' w') < 1
    (Just w, Just h) ->
      let (w'', h'') = containRule (Just w) (Just h) (w', h')
      in w'' == w && h'' >= h || h'' == h && w'' >= w

testResizeRules :: Spec
testResizeRules = do
  describe "resize rules:" $ do
    prop "'contain' resize rule" $ forAll testData containRuleProp
    prop "'cover' resize rule" $ forAll testData coverRuleProp
