----------------------------------------------------------------------------
-- |
-- Module      :  Graphmind.Help
-- Copyright   :  (c) Braden Shepherdson 2009
-- License     :  BSD3
--
-- Maintainer  :  Braden.Shepherdson@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Online help documentation for Graphmind
--
-----------------------------------------------------------------------------

module Graphmind.Help (
  cmdHelp
) where


import Graphmind.Types

import Data.Char (toLower)

import qualified Data.Map as M


type Help = GM ()

cmdHelp :: Command
cmdHelp _ []    = helpIndex
cmdHelp _ (a:_) = case M.lookup (map toLower a) help of
                      Nothing -> helpIndex
                      Just h  -> h



help :: M.Map String Help
help = M.fromList [
   ("quit"     , helpQuit)
  ,("ls"       , helpShowNode)
  ,("show"     , helpShowNode)
  ,("view"     , helpShowNode)
  ,("focus"    , helpFocus)
  ,("fls"      , helpFocus)
  ,("fshow"    , helpFocus)
  ,("pwd"      , helpWhere)
  ,("where"    , helpWhere)
  ,("cd"       , helpMove)
  ,("move"     , helpMove)
  ,("mv"       , helpMove)
  ,("fcd"      , helpMoveFocus)
  ,("fmove"    , helpMoveFocus)
  ,("fmv"      , helpMoveFocus)
  ,("link"     , helpLink)
  ,("unlink"   , helpUnlink)
  ,("edit"     , helpEdit)
  ,("rename"   , helpRename)
  ,("swap"     , helpSwap)
  ,("new"      , helpNew)
  ,("index"    , helpIndex)
  ,("about"    , helpAbout)
  ]


template :: String -> [String] -> Help
template cmd x = io . putStrLn . unlines $ ["", cmd', map (const '=') cmd', ""] ++ x
  where cmd' = "   " ++ cmd ++ "   "


-- | General explanation of graphmind.
helpAbout :: Help
helpAbout = template "About Graphmind" $
  [ "Graphmind is a mind mapping and organization tool based on undirected"
  , "graphs. An undirected graph is a collection of nodes with bidirectional"
  , "links between them. Each Graphmind node has a title, neighbouring nodes,"
  , "and an optional body text."
  , ""
  , "Graphmind is deliberately freeform: your nodes can be people, places, events,"
  , "projects, schedule items, bugs, pet peeves. The structure need not be"
  , "hierarchical, cycles are not only allowed but encouraged."
  , ""
  , "As a user, you interact with Graphmind through commands. Information on those"
  , "commands can be found by typing 'help index'."
  , ""
  , "You navigate Graphmind's graph using two 'cursors', called focus and view."
  , "The focus is considered your central point, your base camp."
  , "In contrast, the view is the node you're currently looking at. Its name is"
  , "displayed in the prompt, and it is the one you'll rename or edit using those"
  , "commands."
  , ""
  , "Graphmind was written in Haskell by Braden Shepherdson."
  ]


helpQuit :: Help
helpQuit = template "quit" $
  [ "Exits from Graphmind."
  , ""
  , "Graphmind saves its data after every change, so there is no 'save' commnad."
  ]


helpShowNode :: Help
helpShowNode = template "show, ls, view" $
  [ "Displays the full details of the current view node." ]


helpFocus :: Help
helpFocus = template "fshow, fls, focus" $
  [ "Displays the full details of the current focus node." ]


helpWhere :: Help
helpWhere = template "where, pwd" $ 
  [ "Displays the titles of both the view and focus nodes." ]


helpMove :: Help
helpMove = template "move, cd, mv" $
  [ "Moves the view cursor to point at the specified node."
  , ""
  , "Nodes can be named in three ways:"
  , "1. By number from the list displayed by show, view and ls."
  , "2. By a unique infix of the title of a neighbour."
  , "   (not case-sensitive, spaces allowed)"
  , "3. One of the magic names 'focus' and 'view', which will move to"
  , "   that node. (Of course, 'move view' won't change anything.)"
  ]


helpMoveFocus :: Help
helpMoveFocus = template "fmove, fcd, fmv" $
  [ "Moves the focus cursor to point at the specified node."
  , ""
  , "Nodes can be named in three ways:"
  , "1. By number from the list displayed by fshow, focus and fls."
  , "2. By a unique infix of the title of a neighbour."
  , "   (not case-sensitive, spaces allowed)"
  , "3. One of the magic names 'focus' and 'view', which will move to"
  , "   that node. (Of course, 'move focus' won't change anything.)"
  ]


helpLink :: Help
helpLink = template "link" $
  [ "Connects the focus and view nodes together."
  , ""
  , "If they are already connected, 'link' does nothing."
  ]


helpUnlink :: Help
helpUnlink = template "unlink" $
  [ "Disconnects the focus and view nodes."
  , ""
  , "If they are not connected, 'unlink' does nothing."
  ]


helpEdit :: Help
helpEdit = template "edit" $
  [ "Allows you to update the body text of the view node." ]


helpRename :: Help
helpRename = template "rename" $
  [ "Allows you to change the title of the view node."
  , ""
  , "The new name can be given following the command."
  , "If no new name is given with the command, rename prompts for one."
  ]


helpSwap :: Help
helpSwap = template "swap" $
  [ "Exchanges the focus and view nodes." ]


helpNew :: Help
helpNew = template "new" $
  [ "Creates a new node and links it to the focus node. The view cursor is moved"
  , "to point to the new node."
  , ""
  , "The title of the new node should be provided following the command, eg."
  , "> new My New Node"
  ]


helpIndex :: Help
helpIndex = template "Help Index" $
  [ "Following is a list of help topics."
  , "For general information about Graphmind, see 'help about'."
  , ""
  , "To view a help topic, use the command"
  , "> help foo"
  , "where 'foo' is one of the following:"
  , ""
  ]
  ++ M.keys help

