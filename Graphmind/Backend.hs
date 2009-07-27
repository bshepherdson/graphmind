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

-- convenience function to extract the Connection and lift from IO
gmQuickQuery :: String -> [SqlValue] -> GM [[SqlValue]]
gmQuickQuery sql params = gets conn >>= \c -> io (quickQuery' c sql params)


-- | Given a node ID, retrieve it and return a Node.
--
-- Requires 2 database queries
getNode :: NodeId -> GM Node
getNode n = do
  [sql] <- gmQuickQuery "SELECT _id, title, text FROM Node WHERE _id = ?" [toSql n]
  let node = Node { _id = fromSql $ sql !! 0, title = fromSql $ sql !! 1, text = fromSql $ sql !! 2, adjacent = [] }
  adjs  <- gmQuickQuery "SELECT Node._id, Node.title FROM Link INNER JOIN Node ON Link.node_to = Node._id WHERE Link.node_from = ?" [toSql $ _id node]
  return node { adjacent = map (\[x,y] -> (fromSql x, fromSql y)) adjs }




