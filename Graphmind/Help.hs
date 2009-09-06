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
  helpMap 
  ,helpAbout
  ,Help
) where


import Graphmind.Util

import Network.FastCGI
import Text.XHtml hiding (title,text,target,pre,content)

import qualified Data.Map as M

type Help = CGI CGIResult


helpMap :: M.Map String Help
helpMap = M.fromList [
           ("About", helpAbout)
          ]


helpAbout :: Help
helpAbout = cgiPg $ cgiPgTemplate "About Graphmind" $
  paragraph << s2h "Graphmind is a mind mapping tool based on undirected graphs. An undirected graph is a collection of nodes with bidirectional links between them. In Graphmind, each node has a title, its neighbouring nodes, and an optional body text."
  +++
  paragraph << s2h "Graphmind is deliberately freeform: your nodes can be people, places, events, projects, schedule items, bugs, pet peeves. The structure need not be hierarchical, cycles are not only allowed but encouraged."
  +++
  h3 << s2h "Navigating the Nodes"
  +++
  paragraph << (
    s2h "When viewing a node, there are links to each of its neighbour nodes. If you click one of these, you will move to viewing that node. In addition to the node you're currently viewing, Graphmind tracks a node known as the "
    +++ bold << s2h "anchor"
    +++ s2h ". The main function of the anchor is when you need to specify two nodes for an operation, such as Link and Unlink. These operations link and unlink the currently viewed node and the anchor node."
    )
  +++
  paragraph << (s2h "There is a section of controls for the anchor on the main node viewing page. You can:"
  +++
  unordList [s2h "Swap the anchor and viewed nodes, so that the node you're viewing now becomes the anchor, and you switch to viewing the node that was the anchor."
            ,s2h "Go to the anchor, so that you're viewing it."
            ,s2h "Move the anchor to the current node."
            ]
   )
  +++
  h3 << "Linking and Unlinking"
  +++
  paragraph << s2h "As mentioned in passing above, to Link and Unlink nodes you must set one of the nodes you wish to link or unlink to the anchor, and then view the other. If the nodes are not currently linked, you will have a \"Link to anchor\" option. If they are currently linked, you will have \"Unlink from anchor\" instead."
  +++
  h3 << s2h "Editing Nodes"
  +++
  paragraph << s2h "Nodes can be edited, updating their body text and titles. They can be deleted too, and you can create new nodes. New nodes are created with a link to the node you were viewing when you clicked \"Create new node\"."
  +++
  h3 << s2h "Further Navigation"
  +++
  paragraph << s2h "You can also search for nodes, and Graphmind will locate nodes containing the search phrase in their title or text. Also there is a search-like function that will locate \"orphaned\" nodes. An orphaned node is one that no longer connects to any others, and therefore couldn't be reached without using a Search."
  +++
  paragraph << s2h "This is not the proper definition of orphaned nodes, which should be any node unreachable from the user's current location, even if those orphaned nodes are connected among themselves but disconnected from the part of the graph where the user is currently. The algorithm for this is well-understood, but it has not been implemented in Graphmind yet."


