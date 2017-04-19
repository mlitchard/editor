-- |
-- Module: src-test/Main.hs
-- Author: Michael Litchard
-- Maintainer: <Michael Litchard> <litchard.michael@gmail.com>
-- A few tests for the editor project

module Main (main) where

import           BasicPrelude hiding (readFile,throwIO,writeFile)
import           Test.Hspec (hspec)
import           System.Directory ( createDirectoryIfMissing
                                  , removeDirectoryRecursive)
import           PropTests
import           UnitTests.UnitTests
import           UnitTests.Scaffolding

-- |
-- Tests are first divided up into unit and property tests.

main :: IO ()
main = do
  createDirectoryIfMissing False "data/test_results"
  res_one   <- prepOne
  res_two   <- prepTwo
  res_three <- prepThree
  res_four  <- prepFour
  hspec $ unitTests (res_one,res_two,res_three,res_four)
  hspec deleteTest
  hspec appendTest
  removeDirectoryRecursive "data/test_results"
