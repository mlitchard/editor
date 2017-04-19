-- |
-- Module: Types
-- Author: Michael Litchard
-- Maintainer: ,Michael Litchard> <litchard.michael@gmail.com>
-- Types and type synonyms for the editor

{-# LANGUAGE GADTs #-}
module Types where

import BasicPrelude

import Data.Conduit (Sink,Conduit)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.State.Strict (StateT)

import System.IO (Handle)

-- | Type used to track undo actions
data Edit = Append (Seq Char) | Delete Int deriving (Show,Eq)

-- | Hold editor state
data Editor = Editor
  { store_edt :: Seq Char
  , undo_edt  :: [Edit]
  , out_handle :: Handle
  } deriving (Show,Eq)

-- | using synonyms here as documention

type Command = Either Int (Int,Text)
type EditorState = StateT Editor IO
type EditorM = ResourceT EditorState
type CommandSink = Sink Command EditorM ()
type ParseConduit = Conduit Text EditorM Command
 
