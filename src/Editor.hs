{-# LANGUAGE Rank2Types #-}
-- |
-- Module: src/Editor.hs
-- Contains the driver plus conduits managing the streaming
-- Author: Michael Litchard

module Editor 
  ( 
    -- The pipeline for each Conduit.
    editor
  , -- initial editor record
    edt
  ) where

import           BasicPrelude hiding (lines)

import           System.IO (stdout,Handle,hClose)
import           Safe (lookupJustNote)

import           Data.Conduit ( Producer,runConduitRes
                              , (.|),bracketP,awaitForever,yield)
import           Data.Conduit.Combinators (sourceHandle)
import           Data.Conduit.List (isolate)
import           Data.Conduit.Text (decode,utf8,lines)
import           Control.Monad.Trans.Resource (MonadResource)

import           Actions
import           Types

-- | Pipeline for editor. Returns final editor state.
-- Note: The isolate function serves as "take"
-- It constrains how many lines are read via sourceLine
editor :: Int -> Handle -> EditorState ()
editor rpt hdl = runConduitRes $ 
                   sourceLine hdl .| 
                   decode utf8    .|
                   lines          .|
                   parseConduit   .|
                   isolate rpt    .| 
                   commandConduit 

-- | Manages handle cleanup.
--   Sends line downstream.
sourceLine :: MonadResource m => Handle -> Producer m ByteString
sourceLine hdl = bracketP (return hdl) hClose sourceHandle
 
-- | Takes ByteString from upstream and transforms it into data needed 
--   for command lookup downstream.
parseConduit :: ParseConduit
parseConduit = awaitForever parse

-- | helper for parseConduit
--   Differentiates between unary and nullary commands
parse :: Text -> ParseConduit
parse t = if str == mempty
          then yield (Left c_int)
          else yield (Right (c_int,unwords str))
  where
    (cmd:str) = words t
    c_int :: Int
    c_int = read cmd

-- | Takes lookup data from upstream and executes command
commandConduit :: CommandSink
commandConduit = awaitForever processCommand
 
-- | helper for commandConduit
-- FIXME: duplicate code
processCommand :: Command -> CommandSink
processCommand (Left cmd) = lift f
  where
    f = lookupJustNote err_msg cmd nullaryCommands
    err_msg = "Err: Command Not Found - " <> show cmd
processCommand (Right (cmd, str)) = lift (f str)
  where 
    f = lookupJustNote err_msg cmd unaryCommands
    err_msg = "Err: Command Not Found - " <> show cmd

-- | Utilities

-- Below are the key value pairs mapping commands to corresponding functions
-- See Actions.hs for details.     
unaryCommands :: [(Int,Text -> EditorM ())]
unaryCommands = [(1,processAppend)
                ,(2,processDelete)
                ,(3,processPrint)]

nullaryCommands :: [(Int,EditorM ())]
nullaryCommands = [(4, processUndo)]

-- Initial editor
edt :: Editor
edt = Editor { store_edt = mempty
             , undo_edt  = []
             , out_handle = stdout
             }
