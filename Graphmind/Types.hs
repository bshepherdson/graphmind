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
  module Control.Monad.Reader
 ,module Control.Applicative
 ,Node(..)
 ,NodeId
 ,GM(..)
 ,GMState(..)
 ,runGM
 ,io
 ,UserId
 ,showNodeList
) where

import Control.Monad.Reader
import Control.Applicative

import Database.HDBC ()
import Database.HDBC.Sqlite3

import Network.FastCGI

type NodeId = Int
type UserId = Int

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


data GMState = GMState { 
    conn    :: !Connection
   ,view    :: !Node
   ,focus   :: !Node
   ,list    :: ![(NodeId,String)]
}

-- | The Graphmind Monad
newtype GM a = GM (ReaderT GMState (CGIT IO) a)
  deriving (Functor, Monad, MonadReader GMState, MonadIO)

runGM :: forall a . GM a -> GMState -> CGIT IO a
runGM (GM a) s = runReaderT a s

io :: forall a. IO a -> GM a
io = liftIO

cgi :: forall a. CGI a -> GM a
cgi = GM . lift

