{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, ExistentialQuantification #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Graphmind.Types
-- Copyright   :  (c) Braden Shepherdson 2009
-- License     :  BSD3
--
-- Maintainer  :  Braden.Shepherdson@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Types used internally by Graphmind.
--
-----------------------------------------------------------------------------

module Graphmind.Types (
  module Control.Monad
 ,module Control.Monad.Reader.Class
 ,module Control.Monad.State.Class
 ,module Control.Applicative
 ,Node(..)
 ,NodeId
 ,GM(..)
 ,GMState(..)
 ,GMConf(..)
 ,runGM
 ,io
 ,cgi
 ,UserId
 ,showNodeList
 ,Pre
 ,Pg
) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.State.Strict
import Control.Monad.State.Class
import Control.Applicative

import Database.HDBC ()
import Database.HDBC.Sqlite3

import qualified Data.Map as M

import Network.FastCGI

type NodeId = Int
type UserId = Int

type Pre = GM ()
type Pg  = GM CGIResult

-- | the basic Node type used to 
data Node = Node { 
    _id      :: !NodeId
   , title    :: !String
   , text     :: Maybe String
   , adjacent :: ![(NodeId,String)] -- IDs and names
} deriving (Read)


instance Show Node where
  show (Node _ t x a) = unlines $ [
      ""
    , t'
    , map (const '=') t'
    , ""
    , "Adjacent nodes:"
    ]
    ++ showNodeList a
    ++ case x of
         Nothing -> []
         Just x' -> ["", x' ]
   where t' = "   " ++ t ++ "   "


showNodeList :: [(NodeId, String)] -> [String]
showNodeList = map (\(n,y) -> show n ++ ". " ++ y) . zip [1::Int ..] . map snd

instance Eq Node where
  (Node x _ _ _) == (Node y _ _ _) = x == y -- equality is just based on IDs


data GMConf = GMConf { 
    conn    :: !Connection
   ,user    :: !UserId
}

data GMState = GMState {
    errors  :: [String]
   ,vars    :: M.Map String String
}

-- | The Graphmind Monad
newtype GM a = GM (ReaderT GMConf (StateT GMState (CGIT IO)) a)
  deriving (Functor, Monad, MonadReader GMConf, MonadState GMState, MonadIO)

runGM :: forall a . GM a -> GMConf -> GMState -> CGIT IO a
runGM (GM a) c st = evalStateT (runReaderT a c) st

io :: forall a. IO a -> GM a
io = liftIO

cgi :: forall a. CGI a -> GM a
cgi = GM . lift . lift


