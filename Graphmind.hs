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
import Graphmind.Help

import qualified Data.Map as M

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.Char (toLower)
import Data.List
import Data.Maybe (maybeToList, fromJust, isNothing)

import System.Exit (exitSuccess, ExitCode(..), exitWith)
--import Control.Exception (handle)
import System.Process (runInteractiveProcess)
import System.Environment (getArgs)
import System.IO


-- main GM monad parsing system

commands :: M.Map String Command
commands = M.fromList [
   ("quit"     , cmdQuit)
  ,("ls"       , cmdShowNode)
  ,("show"     , cmdShowNode)
  ,("view"     , cmdShowNode)
  ,("focus"    , cmdFocus)
  ,("fls"      , cmdFocus)
  ,("fshow"    , cmdFocus)
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
  ,("new"      , cmdNew)
  ,("help"     , cmdHelp)
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
-- Commands: @ls@, @show@, @view@
cmdShowNode :: Command
cmdShowNode _ _ = gets view >>= io . print

-- | Equivalent of 'ShowNode' for the focused node.
--
-- Commands: @fls@, @focus@
cmdFocus :: Command
cmdFocus _ _ = gets focus >>= io . print


-- | Displays just the names of the current nodes.
--
-- Commands: @pwd@, @where@
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
-- Commands: @cd@, @move@, @mv@
cmdMove :: Command
cmdMove c []   = internalMoveUsage c []
cmdMove c args = do
  mn <- locateNode (gets view) args
  case mn of
    Nothing -> io $ putStrLn $ "Can't move, error identifying target node."
    Just n  -> do 
      modify $ \s -> s { view = n }
      io $ putStrLn $ "View node moved to '" ++ title n ++ "'"


-- | Internal function for handling the usage of the various movement commands generically.
internalMoveUsage :: Command
internalMoveUsage c []   = do
  io $ putStrLn $ "Usage: " ++ c ++ " <adjacent node>"
  io $ putStrLn $ "Where an adjacent node can be:"
  io $ putStrLn $ "* A number as given by ls, show or view;"
  io $ putStrLn $ "* A unique part of the name (not case-sensitive);"
  io $ putStrLn $ "* One of the magic nodes 'view' or 'focus'."


-- | Given a @(GM Node)@, searches that Node's adjacent list for a uniquely identified node.
-- See the documentation for 'cmdMove' or 'cmdMoveFocus' for an explanation of how nodes may be identified.
locateNode :: (GM Node) -> [String] -> GM (Maybe Node)
locateNode extract args = do
  f <- gets focus
  v <- gets view
  relevant <- extract
  let adj    = adjacent relevant
      arg    = map toLower $ unwords args -- FIXME: unwords is imperfect here, it might reduce multiple spaces to one space in a node name.
      byName = filter ((arg `isInfixOf`) . map toLower . snd) adj
      byNum  = case maybeRead arg of
                 Nothing -> []
                 Just n  -> maybeToList . lookup n . zip [1..] $ adj
      byMark = [ y | (x,y) <- [("view", (_id v, title v)),("focus", (_id f, title f))], x == arg ]
  case byName ++ byNum ++ byMark of
    [] -> io (putStrLn $ "No matches found for node: '" ++ arg ++ "'.") >> return Nothing
    [x] -> do
      n <- getNode $ fst x
      case n of
        Nothing -> io (putStrLn $ "Could not find a node with ID " ++ show (fst x) ++ " in the database.") >> return Nothing
        Just _ -> return n
    xs -> do 
      io $ putStrLn $ "Ambigious match for node '"++ arg ++"'. Candidates: "
      mapM_ (io . putStrLn) (map snd xs)
      return Nothing


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
-- Commands: @fcd@, @fmove@, @fmv@
cmdMoveFocus :: Command
cmdMoveFocus c [] = internalMoveUsage c []
cmdMoveFocus c args = do
  mn <- locateNode (gets focus) args
  case mn of
    Nothing -> io $ putStrLn $ "Can't move, error identifying target node."
    Just n  -> do 
      modify $ \s -> s { focus = n }
      io $ putStrLn $ "Focus node moved to '" ++ title n ++ "'"


-- | Links nodes together.
--
-- Links the 'view' and 'focus' nodes together. 
-- If they're already linked, this is a no-op, and will say so.
--
-- TODO: Allow an argument to connect an arbitrary node to the 'view' node.
--
-- Commands: @link@
cmdLink :: Command
cmdLink _ _ = do
  v <- gets view
  f <- gets focus
  case lookup (_id f) (adjacent v) of
    Just _  -> io $ putStrLn $ "There is already a link between view ('" ++ title v ++ "') and focus ('" ++ title f ++ "')."
    Nothing -> do
      putNode $ v { adjacent = (_id f, title f) : adjacent v }
      gmCommit
      v' <- gets view
      f' <- gets focus
      io $ putStrLn $ "View ('" ++ title v' ++ "') and focus ('" ++ title f' ++ "') are now linked."



-- | Unlinks nodes. Specifically, the 'view' and 'focus' nodes.
--
-- If they aren't connected, this command will say so and do nothing.
--
-- Commands: @unlink@
cmdUnlink :: Command
cmdUnlink _ _ = do
  v <- gets view
  f <- gets focus
  case lookup (_id f) (adjacent v) of
    Nothing -> io $ putStrLn $ "No link exists between view ('" ++ title v ++ "') and focus ('" ++ title f ++ "')."
    Just x  -> do
      putNode $ v { adjacent = adjacent v \\ [(_id f, title f)] }
      gmCommit
      v' <- gets view
      f' <- gets focus
      io $ putStrLn $ "View ('" ++ title v' ++ "') and focus ('" ++ title f' ++ "') are no longer linked."



-- | Updates the text of the 'view' node.
--
-- Currently just lets you type in a new body 'text'.
--
-- TODO: Invoke the user's @$EDITOR@ instead!
--
-- Commands: @edit@
cmdEdit :: Command
cmdEdit _ _ = do
  v <- gets view
  io $ putStrLn $ "Enter new body text: "
  s <- io $ getLine
  putNode $ v { text = if null s then Nothing else Just s }
  gmCommit
  v' <- gets view
  io $ putStrLn $ "Body text of the view node ('" ++ title v' ++ "') updated."



-- | Changes the title of the 'view' node.
--
-- Commands: @rename@
cmdRename :: Command
cmdRename _ [] = do
  v <- gets view
  io $ putStrLn $ "Enter new title for the view node (currently '" ++ title v ++"'): "
  s <- io $ getLine
  cmdRename undefined [s]
cmdRename _ xs = do
  v <- gets view
  let s = unwords xs
  putNode $ v { title = s }
  gmCommit
  v' <- gets view
  io $ putStrLn $ "Title of the view node changed from '"++ title v ++"' to '" ++ title v' ++ "'."


-- | Exchanges the 'view' and 'focus' nodes.
--
-- Commands: @swap@
cmdSwap :: Command
cmdSwap _ _ = do
  modify $ \s -> s { view = focus s, focus = view s }
  io $ putStrLn $ "Swapped view and focus nodes:"
  cmdWhere "swap" []


-- | Create a new node attached to the 'focus' node, and move the view to the new one.
--
-- Prompts for a title and creates this new node.
--
-- Commands: @new@
cmdNew :: Command
cmdNew _ _ = do
  f <- gets focus
  io $ putStrLn $ "Enter the title for the new node: "
  s <- io getLine
  putNode $ Node { _id = 0, title = s, text = Nothing, adjacent = [(_id f, title f)] }
  f' <- gets focus
  let newID = maximum . map fst . adjacent $ f'
  new <- fromJust <$> getNode newID
  modify $ \s -> s { view = new }
  io $ putStrLn $ "Created new node '" ++ s ++ "'."

-----------------------------------------------------------------------------------------
-- main and company
-----------------------------------------------------------------------------------------

usage :: IO ()
usage = do
  putStrLn $ unlines [
     "Usage: graphmind [OPTIONS] <database file>"
    ,"  Options:"
    ,"  --help, -h      Display this help message."
    ,"  --new <db file> Create a new database with the given name."
    ]

newDB :: FilePath -> IO ()
newDB db = do
  --handle (\e -> putStrLn ("Error attempting to create database: " ++ show e) >> exitWith (ExitFailure 1)) $ do
    (pin,_,_,p) <- runInteractiveProcess "sqlite3" ["-batch" , db] Nothing Nothing
    hPutStrLn pin schema
    hClose pin
    putStrLn $ "Successfully created database '" ++ db ++ "'."


start :: FilePath -> IO ()
start db = do
  putStrLn $ "Graphmind 0.0.1 (c) Braden Shepherdson 2009"
  c <- connectSqlite3 db
  let fakeNode = Node { _id = -1, title = "Fake Node", text = Nothing, adjacent = [] }
  runGM loop (GMState { conn = c, view = fakeNode, focus = fakeNode })



-- the main loop
loop :: GM ()
loop = do
  v <- getNode 1
  when (isNothing v) $ putNode Node { _id = 1, title = "Default Node", text = Nothing, adjacent = [] }
  (Just v') <- getNode 1
  modify $ \s -> s { view = v', focus = v' }
  forever $ do
    v'' <- gets view
    io $ putStr $ title v'' ++ "> "
    io $ hFlush stdout
    s <- io $ getLine
    parser s



-- connect to the DB, and then launch the main loop.
main = do
  args <- getArgs
  case args of
    [] -> usage
    ("--help":_) -> usage
    ("-h":_) -> usage
    ("--new":db:_) -> newDB db
    (db:[]) -> start db
    _ -> usage

  

