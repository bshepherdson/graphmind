{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, ExistentialQuantification #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Graphmind.Types
-- Copyright   :  (c) Braden Shepherdson 2009
-- License     :  BSD3
--
-- Maintainer  :  Braden.Shepherdson@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Types used internally by Graphmind.
--
-----------------------------------------------------------------------------

module Graphmind.Types (
  module Control.Monad.State
 ,module Control.Applicative
 ,Node(..)
 ,NodeId
 ,GM(..)
 ,GMState(..)
 ,runGM
 ,io
 ,
) where

import Control.Monad.State
import Control.Applicative

import Database.HDBC
import Database.HDBC.Sqlite3

type NodeId = Int

-- | the basic Node type used to 
data Node = Node { 
    _id      :: !NodeId
   , title    :: !String
   , text     :: Maybe String
   , adjacent :: ![(NodeId,String)] -- IDs and names
} deriving (Read, Show)



instance Eq Node where
  (Node x _ _ _) == (Node y _ _ _) = x == y -- equality is just based on IDs


data GMState = GMState { 
    conn    :: !Connection
   ,view    :: !Node
   ,focus   :: !Node
}

-- | The Graphmind Monad
newtype GM a = GM (StateT GMState IO a)
  deriving (Functor, Monad, MonadState GMState, MonadIO)

runGM :: forall a . GM a -> GMState -> IO a
runGM (GM a) s = evalStateT a s

io :: forall a. IO a -> GM a
io = liftIO


