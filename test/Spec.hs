module Main where

import Test.Hspec (describe, hspec)
import Tests.Helpers.ResizeRules
import Tests.MockImageLibrary (testMockImageLibrary)
-- import Tests.Resize (testImageResize)

main :: IO ()
main =
  hspec $ do
    describe "Testing helper functions:" testResizeRules
    describe "Testing mock image library:" testMockImageLibrary
    -- describe "Testing resize logic:" testImageResize
