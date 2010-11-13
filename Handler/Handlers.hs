{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Handler.Handlers where

import Graphmind
import Data.Maybe
import Data.Int
import Control.Applicative
import Control.Monad
import Control.Arrow ((***))

import Data.List (sortBy)
import Data.Ord (comparing)

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- Graphmind.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        setTitle $ "Graphmind homepage"
        addWidget $(hamletFile "homepage")



getAnchorR :: Handler RepHtml
getAnchorR = do
  (uid, u) <- requireAuth
  case userAnchor u of
    Nothing  -> notFound
    Just aid -> nodeHandler uid u aid


getViewR :: Int64 -> Handler RepHtml
getViewR nid_ = do
  (uid, u) <- requireAuth
  -- check the node in question actually belongs to this user
  let nid = toPersistKey nid_
  mn <- runDB $ get nid
  when (isNothing mn) notFound -- short-circuit
  let n = fromJust mn -- safe, guarded by the above
  when (uid /= nodeOwner n) notFound -- not a 403, don't want to give away what exists and what doesn't (TODO: too paranoid?)
  nodeHandler uid u nid


nodeHandler :: UserId -> User -> NodeId -> Handler RepHtml
nodeHandler uid u nid = do
  let aid = fromJust $ userAnchor u -- TODO: Unsafe
  (manchor, mnode) <- runDB $ do
    a <- get aid
    n <- get nid
    return (a,n)
  when (isNothing mnode) notFound
  let node      = fromJust mnode -- safe
      body      = fmap T.unpack $ nodeBody node
      anchor    = fromJust manchor
      isAnchor  = aid == nid
  links <- runDB $ do
    links <- selectList [LinkFromEq nid] [] 0 0
    justnodes <- mapM (get . linkTo . snd) links
    let nodes = catMaybes $ zipWith (liftA2 (,)) (map (Just . linkTo . snd) links) justnodes
    --nodes <- map (id *** fromJust) . filter (isJust.snd) <$> mapM (\x -> (x, get . linkTo . snd $ x)) links :: YesodDB y (GHandler sub y) [(Key Node, Node)]
    return $ sortBy (comparing (nodeTitle.snd)) nodes -- can't be sorted in the DB given the current implementation of Persistent
  defaultLayout $ do
    setTitle $ string $ nodeTitle node
    addWidget $(hamletFile "node")
    -- let anchor = fromJust manchor
    -- addWidget $ actionsWidget anchor node
    -- addWidget $ anchorWidget anchor node
    -- addWidget $ searchWidget



actionsWidget :: Node -> Node -> Widget ()
actionsWidget anchor node = [$hamlet|
%h3 Actions
|]
-- %ul
--    %li %a!href=@NewR@ 

anchorWidget :: Node -> Node -> Widget ()
anchorWidget anchor node = [$hamlet|
%h3 Anchor
|]


searchWidget :: Widget ()
searchWidget = [$hamlet|
%h3 Search
|]


{-
actionWidget :: Node -> Node -> Html
actionWidget a n = h3 << s2h "Actions" +++ unordList (map snd . filter fst $ [
     (True, gmlink "" "New" [("parent", show (_id n))] (s2h "Create new node"))
    ,(True, gmlink "" "Edit" [("edit", show (_id n))] (s2h "Edit this node"))
    ,(True, gmlink "" "Delete" [("delete", show (_id n))] (s2h "Delete this node"))
    ,(a /= n && not neighbours, 
        gmlink "Link" "View" [("link1", show (_id a)), ("link2", show (_id n)), ("view", show (_id n))] (s2h "Link to anchor"))
    ,(neighbours, gmlink "Unlink" "View" [("link1", show (_id a)), ("link2", show (_id n)), ("view", show (_id n))] (s2h "Unlink from anchor"))
    ,(True, gmlink "" "Orphans" [] (s2h "Orphaned Nodes"))
    ])
  where neighbours = isJust . lookup (_id n) . adjacent $ a
-}


