-- |
-- Module: src-test/UnitTests/Scaffolding.hs
-- Author: Michael Litchard
-- Maintainer: <Michael Litchard> <litchard.michael@gmail.com>
-- Scaffolding for the unit tests
--
module UnitTests.Scaffolding (prepOne,prepTwo,prepThree,prepFour) where

import           BasicPrelude hiding (writeFile,readFile,replicate)

import           Control.Monad.State.Strict (execStateT)

import           Data.Text (append,replicate)

import           Data.Text.IO (writeFile,readFile,hGetLine)
import           System.IO ( Handle,hClose,FilePath
                           , openBinaryFile,IOMode (ReadMode,WriteMode))

import           Editor hiding (edt)
import           Types (Editor (..))

prepOne :: IO Text
prepOne = do
  in_h  <- openBinaryFile tc_one_data ReadMode
  out_h <- openBinaryFile tc_one_res WriteMode
  lpn <- read <$> hGetLine in_h :: IO Int
  let test_edt = testEditor out_h
  _ <- execStateT (editor lpn in_h) test_edt
  hClose out_h
  readFile tc_one_res

prepTwo :: IO Text
prepTwo = do
  in_h <- openBinaryFile tc_two_data ReadMode
  out_h <- openBinaryFile tc_two_res WriteMode
  lpn <- read <$> hGetLine in_h :: IO Int
  let test_edt = testEditor out_h
  _ <- execStateT (editor lpn in_h) test_edt
  hClose out_h
  readFile tc_two_res

prepThree :: IO Text
prepThree = do
  in_h <- openBinaryFile tc_three_data ReadMode
  out_h <- openBinaryFile tc_three_res WriteMode
  lpn <- read <$> hGetLine in_h :: IO Int
  let test_edt = testEditor out_h
  _ <- execStateT (editor lpn in_h) test_edt
  hClose out_h
  readFile tc_three_res

prepFour :: IO Text
prepFour = do
  writeFile tc_four_data hugeInput
  in_h <- openBinaryFile tc_four_data ReadMode
  out_h <- openBinaryFile tc_four_res WriteMode
  let edt = testEditor out_h
  lpn <- read <$> hGetLine in_h :: IO Int
  _ <- execStateT (editor lpn in_h) edt
  hClose out_h
  readFile tc_four_res

hugeStr :: Text
hugeStr = "1 " `append` replicate 999999 test_str `append` "\n"

hugeInput :: Text
hugeInput =
   "1000000\n"                `append`
   hugeStr                    `append`
   replicate 499999 "2 1\n" `append`
   replicate 499999 "4\n"   `append`
   "3 1\n"

test_str :: Text
test_str = "thisismyteststring"

testEditor :: Handle -> Editor
testEditor h_out =
  Editor { store_edt = mempty
         , undo_edt  = []
         , out_handle = h_out
         }

tc_one_data :: FilePath
tc_one_data = "data/tc_one.data"
tc_one_res :: FilePath
tc_one_res = "data/test_results/tc_one_res.data"

tc_two_data :: FilePath
tc_two_data = "data/tc_two.data"
tc_two_res :: FilePath
tc_two_res = "data/test_results/tc_two_res.data"

tc_three_data :: FilePath
tc_three_data = "data/tc_three.data"
tc_three_res :: FilePath
tc_three_res = "data/test_results/tc_three_res.data"

tc_four_data :: FilePath
tc_four_data = "data/benchmark_input.data"
tc_four_res :: FilePath
tc_four_res = "data/test_results/tc_four_res.data"
