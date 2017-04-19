-- |
-- Module: PropTests
-- Author: Michael Litchard
-- Maintainer: <Michael Litchard> 
-- Property tests for append and delete

module PropTests (deleteTest,appendTest) where

import BasicPrelude hiding (delete)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (forAll,choose)
import System.IO (stdout)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Types (Editor (..),Edit (..))
import Actions (append,delete)

deleteTest :: Spec
deleteTest = 
  describe "QuickCheck test for delete function"               $
    modifyMaxSuccess (const maxS)                              $
    prop ("deleting as many as " <> show ub <> "characters") $
    forAll (choose (1,ub)) prop_delete
  where ub = Seq.length sample_str
        maxS = 200 :: Int

appendTest :: Spec
appendTest = 
  describe "QuickCheck test for append" $
    prop "QuickCheck test for append"   $
    forAll (choose (0,ub)) prop_append
  where ub = T.length append_text - 1
  
prop_delete :: Int -> Bool
prop_delete lb = model_store == del_store && model_undo == del_undo
  where
    (Editor edt_store edt_undo _)  = editor_p
    (model_store,removed_p)        = Seq.splitAt split_point edt_store
    (Editor del_store del_undo _ ) = delete lb editor_p
    model_undo                     = Append removed_p:edt_undo
    split_point                    = Seq.length edt_store - lb

prop_append :: Int -> Bool
prop_append idx = model_store == append_store && model_undo == append_undo
  where
    (Editor edt_store edt_undo _)       = editor_p
    model_store                         = (Seq.|>) edt_store txt
    (Editor append_store append_undo _) = append (T.pack [txt]) editor_p
    model_undo                          = Delete 1 :edt_undo
    txt                                 = T.index append_text idx

append_text :: Text
append_text = T.pack ['a' .. 'z']

editor_p :: Editor
editor_p = 
  Editor { store_edt = sample_str
         , undo_edt = []
         , out_handle = stdout
         }

sample_str :: Seq.Seq Char
sample_str =
  Seq.fromList "thisisasamplestringtherearemanylikeitbutthisoneismine"
