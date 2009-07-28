{-# LANGUAGE ExistentialQuantification #-}

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


module Main (
  main
) where

import Graphmind.Types
import Graphmind.Backend

import qualified Data.Map as M

import Database.HDBC

import Data.Char (toLower)
import Data.List
import Data.Maybe (maybeToList)

import System.Exit (exitSuccess)


-- main GM monad parsing system

type Command = String -> [String] -> GM ()

commands :: M.Map String Command
commands = M.fromList [
   ("quit"     , cmdQuit)
  ,("ls"       , cmdShowNode)
  ,("show"     , cmdShowNode)
  ,("view"     , cmdShowNode)
  ,("focus"    , cmdFocus)
  ,("fls"      , cmdFocus)
  ,("pwd"      , cmdWhere)
  ,("where"    , cmdWhere)
  ,("cd"       , cmdMove)
  ,("move"     , cmdMove)
  ,("mv"       , cmdMove)
  ,("fcd"      , cmdMoveFocus)
  ,("fmove"    , cmdMoveFocus)
  ,("fmv"      , cmdMoveFocus)
  ,("link"     , cmdLink)
  ,("unlink"   , cmdUnlink)
  ,("edit"     , cmdEdit)
  ,("rename"   , cmdRename)
  ,("swap"     , cmdSwap)
  ]


-- utility functions
maybeRead :: forall a . (Read a) => String -> Maybe a
maybeRead s = case reads s of
                [(x,"")] -> Just x
                _        -> Nothing




parser :: String -> GM ()
parser s = do
  let args = words s
  case args of
    []       -> return ()
    (cmd:as) ->
      case M.lookup cmd commands of
        Nothing -> io $ putStrLn $ "Unknown command: '" ++ cmd ++ "'"
        Just c  -> c cmd as



cmdQuit :: Command
cmdQuit _ _ = io (putStrLn "Goodbye") >> shutdown


shutdown :: GM ()
shutdown = do
  c <- gets conn
  io $ commit c
  io $ disconnect c
  io $ exitSuccess


-- | Shows the currently viewed node.
--
-- Commands: ls, show, view
cmdShowNode :: Command
cmdShowNode _ _ = gets view >>= io . print

-- | Equivalent of 'ShowNode' for the focused node.
--
-- Commands: fls, focus
cmdFocus :: Command
cmdFocus _ _ = gets focus >>= io . print


-- | Displays just the names of the current nodes.
--
-- Commands: pwd, where
cmdWhere :: Command
cmdWhere _ _ = do
  st <- get
  io $ putStrLn $ "Viewing: " ++ title (view  st)
  io $ putStrLn $ "Focused: " ++ title (focus st)


-- | Moves the 'view' node to the named node.
-- 
-- Nodes can be named in three ways:
--
-- 1. By number from the list displayed in show/view/ls.
--
-- 2. By a unique infix of the title.
--
-- 3. By a unique infix of a bookmarked node's name. Currently these include only the magic names
--    @focus@ and @view@, since bookmarks aren't implemented.
--
-- Commands: cd, move, mv
cmdMove :: Command
cmdMove c []   = internalMoveUsage c []
cmdMove c args = do
  v <- gets view
  f <- gets focus
  internalMove v f (adjacent v) (\n -> modify $ \s -> s { view = n }) "view" c args


-- | Internal function for handling the usage of the various movement commands generically.
internalMoveUsage :: Command
internalMoveUsage c []   = do
  io $ putStrLn $ "Usage: " ++ c ++ " <adjacent node>"
  io $ putStrLn $ "Where an adjacent node can be:"
  io $ putStrLn $ "* A number as given by ls, show or view;"
  io $ putStrLn $ "* A unique part of the name (not case-sensitive);"
  io $ putStrLn $ "* One of the magic nodes 'view' or 'focus'."


-- | Takes the 'view' and 'focus' nodes as the first two arguments, then the list of
-- nodes adjacent to the relevant one, a function that updates the right value in GMState,
-- and the name to use in the output.
internalMove :: Node -> Node -> [(NodeId, String)] -> (Node -> GM ()) -> String -> Command
internalMove v f adj update name _ args = do
  let arg    = map toLower $ unwords args -- FIXME: unwords is imperfect here, it might reduce multiple spaces to one space in a node name.
      byName = filter ((arg `isInfixOf`) . map toLower . snd) adj
      byNum  = case maybeRead arg of
                 Nothing -> []
                 Just n  -> maybeToList . lookup n . zip [1..] $ adj
      byMark = [ y | (x,y) <- [("view", (_id v, title v)),("focus", (_id f, title f))], x == arg ]
  case byName ++ byNum ++ byMark of
    [] -> io $ putStrLn $ "No matches found for node: '" ++ arg ++ "'."
    [x] -> do
      n <- getNode $ fst x
      case n of
        Nothing -> io $ putStrLn $ "Could not find a node with associated ID " ++ show (fst x) ++ " in the database."
        Just n' -> do
          update n'
          io $ putStrLn $ name ++ " is now '" ++ snd x
    xs -> do 
      io $ putStrLn $ "Ambigious match for node '"++ arg ++"'. Candidates: "
      mapM_ (io . putStrLn) (map snd xs)


-- | Moves the 'focus' node to the named node.
-- 
-- Nodes can be named in three ways:
--
-- 1. By number from the list displayed in show/view/ls.
--
-- 2. By a unique infix of the title.
--
-- 3. By a unique infix of a bookmarked node's name. Currently these include only the magic names
--    @focus@ and @view@, since bookmarks aren't implemented.
--
-- Commands: fcd, fmove, fmv
cmdMoveFocus :: Command
cmdMoveFocus c [] = internalMoveUsage c []
cmdMoveFocus c args = do
  v <- gets view
  f <- gets focus
  internalMove v f (adjacent f) (\n -> modify $ \s -> s { view = n }) "focus" c args



cmdLink = undefined
cmdUnlink = undefined
cmdEdit = undefined
cmdRename = undefined
cmdSwap = undefined

main = undefined

