import           BasicPrelude hiding (concat,replicate)

import           Criterion
import           Criterion.Main

import           Data.Text (replicate,append)
import           Data.Text.IO (hGetLine)
import           Types
import           Editor hiding (edt)
import           System.Directory
import           System.IO ( Handle,hClose,openBinaryFile
                           , IOMode (ReadMode,WriteMode))
import           Control.Monad.State.Strict (execStateT)

main :: IO ()
main = defaultMain [ bench "Really big input test" $ whnfIO runEditor ]

runEditor :: IO ()
runEditor = do
  writeFile "benchmark_input.data" hugeInput
  in_h <- openBinaryFile "benchmark_input.data" ReadMode
  out_h <- openBinaryFile "/dev/null" WriteMode
  let edt = testEditor out_h 
  lpn <- read <$> hGetLine in_h :: IO Int
  _ <- execStateT (editor lpn in_h) edt
  hClose out_h
  removeFile "benchmark_input.data"
  return ()

hugeStr :: Text
hugeStr = "1 " `append` replicate 999999 test_str `append` "\n"

hugeInput :: Text
hugeInput =  
   "1000000\n"              `append`
   hugeStr                  `append`
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
