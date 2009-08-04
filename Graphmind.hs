{-# LANGUAGE ExistentialQuantification #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Graphmind
-- Copyright   :  (c) Braden Shepherdson 2009
-- License     :  BSD3
--
-- Maintainer  :  Braden.Shepherdson@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Main file for Graphmind, handles CGI requests.
--
-----------------------------------------------------------------------------


module Main (
  main
) where

import Graphmind.Types
import Graphmind.Backend
import Graphmind.Sessions
import Graphmind.Web

import Network.FastCGI

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.Maybe (fromMaybe)
import qualified Data.Map as M


graphmind :: GM CGIResult
graphmind = do
  preName <- cgi $ fromMaybe "" <$> getInput "pre"
  pgName  <- cgi $ fromMaybe "View"      <$> getInput "pg"

  case (preName,pgName) of
    ("Login",_) -> cgi $ redirect "/graphmind?pg=View" -- redirect, logged-in people shouldn't be logging in again
    (_,"Login") -> cgi $ redirect "/graphmind?pg=View"
    _           -> do
      let pre = fromMaybe (return ()) $ M.lookup preName preMap
          pg  = fromMaybe pgView      $ M.lookup pgName  pgMap
  
      pre
      io . commit =<< asks conn
      pg


cgiMain :: Connection -> CGI CGIResult
cgiMain c = do
  s <- checkSession c
  pre <- getInput "pre"
  case (s,pre) of
    (Nothing, Just "Login") -> preLogin c
    (Nothing, _)            -> pgLogin
    (Just u, _)             -> runGM graphmind (GMState c u)


main :: IO ()
main = do
  c <- connectSqlite3 "/home/braden/haskell/graphmind/test.db"
  runFastCGI $ cgiMain c


