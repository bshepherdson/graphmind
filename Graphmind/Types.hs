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
 ,cgi
 ,UserId
 ,showNodeList
 ,Pre
 ,Pg

 ,logmsg
 ,gmRedirect
) where

import Control.Monad.Reader
import Control.Applicative

import Database.HDBC ()
import Database.HDBC.Sqlite3

import Data.Time

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


data GMState = GMState { 
    conn    :: !Connection
   ,user    :: !UserId
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



-- debugging

logmsg :: String -> IO ()
logmsg s = do
  t <- getCurrentTime
  appendFile "/var/log/graphmind.log" $ "[" ++ show t ++ "] " ++ s ++ "\n"

gmRedirect :: String -> CGI CGIResult
gmRedirect t = setHeader "Location" t >> outputNothing
