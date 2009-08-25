----------------------------------------------------------------------------
-- |
-- Module      :  Graphmind.Backend
-- Copyright   :  (c) Braden Shepherdson 2009
-- License     :  BSD3
--
-- Maintainer  :  Braden.Shepherdson@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Back-end functions for Graphmind, dealing mainly with accessing the
-- database and managing the installation.
--
-----------------------------------------------------------------------------


module Graphmind.Backend 
(
   getNode
  ,putNode
  ,createNode
  ,deleteNode
  ,deleteNodeById
  --,getView
  ,getAnchor
  ,searchNodes
  ,orphanedNodes

  ,setAnchor
)
where

import Database.HDBC
--import Database.HDBC.Sqlite3

import Data.List
import Data.Function (on)
import Data.Maybe (fromJust)

import Graphmind.Types

-- convenience function to extract the Connection and lift from IO
gmQuickQuery :: String -> [SqlValue] -> GM [[SqlValue]]
gmQuickQuery sql params = asks conn >>= \c -> io (quickQuery' c sql params)

gmRun :: String -> [SqlValue] -> GM ()
gmRun sql params = asks conn >>= \c -> io (run c sql params) >> return ()


-- | Given a node ID, retrieve it and return a Node.
--
-- Returns Nothing if that node cannot be found.
getNode :: NodeId -> GM (Maybe Node)
getNode n = do
  u  <- asks user
  rs <- gmQuickQuery "SELECT _id, title, text FROM Node WHERE _id = ? AND user = ?" [toSql n, toSql u]
  case rs of
    [sql] -> do
      let node = Node { _id = fromSql $ sql !! 0, title = fromSql $ sql !! 1, text = fromSql $ sql !! 2, adjacent = [] }
      adjs  <- adjacentNodes (_id node)
      return . Just $ node { adjacent = map (\[x,y] -> (fromSql x, fromSql y)) adjs }
    _     -> return Nothing

adjacentNodes :: NodeId -> GM [[SqlValue]]
adjacentNodes n = do
  u <- asks user
  gmQuickQuery "SELECT Node._id, Node.title FROM Link INNER JOIN Node ON Link.node_to = Node._id WHERE Node.user = ? AND Link.node_from = ?" 
               [toSql u, toSql n]



getSpecialNode :: String -> GM Node
getSpecialNode s = do
  u <- asks user
  rs <- gmQuickQuery ("SELECT " ++ s ++ " FROM User WHERE _id = ?") [toSql u]
  case rs of
    [sql] -> fromJust <$> getNode (fromSql $ sql !! 0)
    _     -> error $ "Couldn't find " ++ s ++ " node."


-- | Gets the user's 'view' node. It must exist, so no Maybe.
--getView :: GM Node
--getView = getSpecialNode "view"

-- | Gets the user's 'anchor' node. It must exist, so no Maybe.
getAnchor :: GM Node
getAnchor = getSpecialNode "anchor"



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
    Nothing -> createNode new >> return ()
    Just o  -> updateNode new o


-- | Creates a new node.
createNode :: Node -> GM NodeId
createNode new = do
  uid <- asks user
  let params = [toSql $ title new, toSql $ text new, toSql uid]
  gmRun "INSERT INTO Node (title, text, user) VALUES (?,?,?)" params
  rs <- gmQuickQuery "SELECT _id, title, text FROM Node WHERE title = ?" $ take 1 params
  let n = head . last $ rs -- grab the _id. note that this handles multiple same-named nodes
  mapM_ (\(i,_) -> gmRun "INSERT INTO Link (node_from,node_to) VALUES (?,?)" [n, toSql $ i]
                >> gmRun "INSERT INTO Link (node_from,node_to) VALUES (?,?)" [toSql $ i, n])
        (adjacent new)
  return $ fromSql n


-- updates the old node to match the new node, with a minimum of database queries
updateNode :: Node -> Node -> GM ()
updateNode new old = do
  uid <- asks user
  when (title new /= title old || text new /= text old) 
    $ gmRun "UPDATE Node SET title = ?, text = ? WHERE _id = ? AND user = ?" 
                   [toSql $ title new, toSql $ text new, toSql $ _id new, toSql uid] 
        >> return ()
  let missing = deleteFirstsBy ((==) `on` fst) (adjacent old) (adjacent new)
      added   = deleteFirstsBy ((==) `on` fst) (adjacent new) (adjacent old)
  mapM_ (\(i,_) -> gmRun "DELETE FROM Link WHERE node_to = ? AND node_from = ?" [toSql $ i, toSql $ _id new]
                >> gmRun "DELETE FROM Link WHERE node_to = ? AND node_from = ?" [toSql $ _id new, toSql $ i])
        missing
  mapM_ (\(i,_) -> gmRun "INSERT INTO Link (node_from,node_to) VALUES (?,?)" [toSql $ i, toSql $ _id new]
                >> gmRun "INSERT INTO Link (node_from,node_to) VALUES (?,?)" [toSql $ _id new, toSql $ i])
        added


deleteNode :: Node -> GM ()
deleteNode n = do
  let snid = toSql $ _id n
  gmRun "DELETE FROM Link WHERE node_to = ? OR node_from = ?" [snid, snid]
  gmRun "DELETE FROM Node WHERE _id = ?" [snid] -- safe because we had to getNode

deleteNodeById :: NodeId -> GM ()
deleteNodeById nid = do
  mn <- getNode nid
  case mn of
    Nothing -> return ()
    Just n  -> deleteNode n


-- | Given a search string, returns all nodes whose title or body text matches
searchNodes :: String -> GM [(NodeId, String)]
searchNodes s = do
  u <- asks user
  let str = "%" ++ s ++ "%"
  rs <- gmQuickQuery "SELECT _id, title FROM Node WHERE user = ? AND (title LIKE ? OR (text NOT NULL AND text LIKE ?))" [toSql u, toSql str, toSql str]
  return $ map (\[i,t] -> (fromSql i, fromSql t)) rs


-- | Returns all orphaned nodes -- that is, all nodes without neighbours.
-- This can still lose a component subgraph that's disconnected from the rest.
orphanedNodes :: GM [(NodeId, String)]
orphanedNodes = do
  u <- asks user
  rs <- gmQuickQuery "SELECT _id, title FROM Node WHERE user = ? AND _id NOT IN (SELECT DISTINCT node_from FROM Link)" [toSql u]
  return $ map (\[i,t] -> (fromSql i, fromSql t)) rs



setAnchor :: NodeId -> GM ()
setAnchor nid = do
  uid <- asks user
  io . logmsg $ "setAnchor: uid = " ++ show uid ++ ", nid = " ++ show nid
  gmRun "UPDATE User SET anchor = ? WHERE _id = ?" [toSql nid, toSql uid]


