-- | Module: src/Actions.hs
-- Stateful actions paired with their helper functions.
-- This is where the editor commands live.

module Actions where

import           BasicPrelude hiding (delete)
import           Control.Monad.State.Strict (liftIO,get,modify)

import qualified Data.Sequence as Seq ((|>),length,splitAt,index,(><))

import           System.IO hiding (print,putStrLn,getLine,utf8)

import qualified Data.Text as T (foldl',length)

import           Types

-- | append operation
-- Also stores diametric delete Edit
-- for undo operation.
processAppend :: Text -> EditorM ()
processAppend txt = modify (append txt)

append :: Text -> Editor -> Editor
append txt edt@(Editor store u_edt _) =
  edt { store_edt = app, undo_edt = undo'}
  where
    app       = T.foldl' (Seq.|>) store txt
    num_chars = T.length txt
    undo'     = Delete num_chars:u_edt

-- | delete operation
-- Also stores diametric append Edit
-- for undo operation.
processDelete :: Text -> EditorM ()
processDelete num_str = modify (delete del_num)
  where del_num = read num_str
    

delete :: Int -> Editor -> Editor
delete del_num edt@(Editor store u_edt _) =
  edt {store_edt = remainder, undo_edt = undo'}
  where
    split_point = Seq.length store - del_num
    (remainder,removed) = Seq.splitAt split_point store
    undo' = Append removed:u_edt

-- | print operation
-- This sends character to Handle stored in
-- the out_handle field of Editor.
processPrint :: Text -> EditorM ()
processPrint num_str = do
  (Editor store _ out_h) <- get
  let char = Seq.index store prt_num
  liftIO (hPutStr out_h $ char:"\n")
  where
    prt_num = (read num_str :: Int) - 1

-- | undo operation
-- This pops and evaluates the diametric Edit.
-- For example, an Append of 'abc' would result in the storing of
-- (Delete 3) in the undo_edt field of Editor.
-- An undo would pop the (Delete 3) and then remove the last
-- three Chars.
processUndo :: EditorM ()
processUndo = modify undo

undo :: Editor -> Editor
undo edt@(Editor _ [] _) = edt
undo edt@(Editor store (undo_act:undo_xs) _) =
  edt { store_edt = remainder, undo_edt = undo_xs }
  where remainder = case undo_act of
          (Append app) -> (Seq.><) store app
          (Delete lb)  -> fst (Seq.splitAt split_point store)
                            where split_point = Seq.length store - lb
