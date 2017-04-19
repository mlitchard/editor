-- | 
-- Module: UnitTests.UnitTest
-- Author: Michael Litchard
-- Maintainer: <Michael Litchard> <litchard.michael@gmail.com>
-- A few unit tests to try and baffle the editor

module UnitTests.UnitTests (unitTests) where

import           BasicPrelude hiding (readFile,replicate)

import           Test.Hspec (it, Spec, shouldBe, describe)

unitTests :: (Text,Text,Text,Text) -> Spec
unitTests (res_one,res_two,res_three,res_four) =
  describe "Three Unit Tests" $ do
    it "Append,and Undo" $ 
      res_one `shouldBe` expected_result_test_1
    it "HackerRank Test Case" $
      res_two `shouldBe` expected_result_test_2
    it "Trying really hard to confuse the editor" $
      res_three `shouldBe` expected_result_test_3
    it "Big Honking File" $
      res_four `shouldBe` expected_result_test_4

expected_result_test_1 :: Text
expected_result_test_1 = "a\nb\nc\nd\ne\nf\nc\ng\nh\nb\nz\ng\n"

expected_result_test_2 :: Text
expected_result_test_2 = "c\ny\na\n"

expected_result_test_3 :: Text
expected_result_test_3 = "c\nd\nf\ng\nj\nd\nc\n"

expected_result_test_4 :: Text
expected_result_test_4 = "t\n"
