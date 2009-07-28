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

import Data.List
import Data.Function (on)

import Graphmind.Types

-- convenience function to extract the Connection and lift from IO
gmQuickQuery :: String -> [SqlValue] -> GM [[SqlValue]]
gmQuickQuery sql params = gets conn >>= \c -> io (quickQuery' c sql params)


-- | Given a node ID, retrieve it and return a Node.
--
-- Returns Nothing if that node cannot be found.
getNode :: NodeId -> GM (Maybe Node)
getNode n = do
  rs <- gmQuickQuery "SELECT _id, title, text FROM Node WHERE _id = ?" [toSql n]
  case rs of
    [] -> return Nothing
    [sql] -> do
      let node = Node { _id = fromSql $ sql !! 0, title = fromSql $ sql !! 1, text = fromSql $ sql !! 2, adjacent = [] }
      adjs  <- adjacentNodes (_id node)
      return . Just $ node { adjacent = map (\[x,y] -> (fromSql x, fromSql y)) adjs }

adjacentNodes n = gmQuickQuery "SELECT Node._id, Node.title FROM Link INNER JOIN Node ON Link.node_to = Node._id WHERE Link.node_from = ?" [toSql n]

-- | Given a Node, writes it to the database. If the node exists but differs
-- from the one stored in the database, a minimally invasive update is performed.
--
-- If this node does not already exist in the database, it is created. In this 
-- case, NULL is passed as the _id value to allow it to be set as a Primary Key.
--
-- NB: In the case of a new node, the _id value /will not match that found in the database/.
putNode :: Node -> GM ()
putNode new = do
  old <- getNode (_id new)
  case old of
    Nothing -> createNode new
    Just o  -> updateNode new o


-- internal (for now?) function to create a new node.
createNode :: Node -> GM ()
createNode new = do
  gmQuickQuery "INSERT INTO Node (title, text) VALUES (?,?)" [toSql $ title new, toSql $ text new]
  mapM_ (\(i,_) -> gmQuickQuery "INSERT INTO Link (node_from,node_to) VALUES (?,?)" [toSql $ _id new, toSql $ i]
                >> gmQuickQuery "INSERT INTO Link (node_from,node_to) VALUES (?,?)" [toSql $ i, toSql $ _id new])
        (adjacent new)


-- updates the old node to match the new node, with a minimum of database queries
updateNode :: Node -> Node -> GM ()
updateNode new old = do
  when (title new /= title old || text new /= text old) 
    $ gmQuickQuery "UPDATE Node SET title = ?, text = ? WHERE _id = ?" 
                   [toSql $ title new, toSql $ text new, toSql $ _id new] 
        >> return ()
  let missing = deleteFirstsBy ((==) `on` fst) (adjacent old) (adjacent new)
      added   = deleteFirstsBy ((==) `on` fst) (adjacent new) (adjacent old)
  mapM_ (\(i,_) -> gmQuickQuery "DELETE FROM Link WHERE node_to = ? AND node_from = ?" [toSql $ i, toSql $ _id new]
                >> gmQuickQuery "DELETE FROM Link WHERE node_to = ? AND node_from = ?" [toSql $ _id new, toSql $ i])
        missing
  mapM_ (\(i,_) -> gmQuickQuery "INSERT INTO Link (node_from,node_to) VALUES (?,?)" [toSql $ i, toSql $ _id new]
                >> gmQuickQuery "INSERT INTO Link (node_from,node_to) VALUES (?,?)" [toSql $ i, toSql $ _id new])
        added


