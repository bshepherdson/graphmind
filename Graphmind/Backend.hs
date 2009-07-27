----------------------------------------------------------------------------
-- |
-- Module      :  Graphmind.Backend
-- Copyright   :  (c) Braden Shepherdson 2009
-- License     :  BSD3
--
-- Maintainer  :  Braden.Shepherdson@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Back-end functions for Graphmind, dealing mainly with accessing the
-- database and managing the installation.
--
-----------------------------------------------------------------------------


module Graphmind.Backend 
(
)
where

import Database.HDBC
import Database.HDBC.Sqlite3

import Graphmind.Types

-- | Given a node ID, retrieve it and return a Node.
--
-- Requires 2 database queries

getNode :: NodeId -> GM Node
getNode n = do
  

