-- |
-- Module: Main.hs
-- Entry point for https://www.hackerrank.com/challenges/simple-text-editor
-- Author: Michael Litchard

import BasicPrelude
import Control.Monad.State.Strict (execStateT)
import System.IO (stdin)

import Editor (editor,edt)

main :: IO ()
main = do
  lpn <- read <$> getLine :: IO Int
  execStateT (editor lpn stdin) edt >> return ()

