{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Handler.Handlers where

import Graphmind
import Data.Maybe
import Data.Int
import Control.Applicative
import Control.Monad
import Control.Arrow ((***))

import Data.List (isInfixOf,sortBy)
import Data.Ord (comparing)
import Data.Char (toLower)

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T


-- helper functions

-- turns a NodeId into a Node. if the Node is not found, does a notFound
getNode :: NodeId -> Handler Node
getNode nid = do
  mnode <- runDB $ get nid
  case mnode of
    Nothing -> notFound
    Just n  -> return n


-- like getNode, but additionally checks that the given UserId is the owner of the Node, returning a permissionDenied otherwise
getNodePerms :: NodeId -> UserId -> Handler Node
getNodePerms nid uid = do
  node <- getNode nid
  when (nodeOwner node /= uid) $ permissionDenied "You are not the owner of this node."
  return node



-- wrapper for defaultLayout that inserts a list of widgets into the sidebar, and a single widget into the content
-- automatically includes the searchWidget
sidebarLayout :: [Widget ()] -> Widget () -> Handler RepHtml
sidebarLayout widgets content = defaultLayout $ do
  addWidget [$hamlet|
%div#left-sidebar
    %h3 Graphmind
    ^searchWidget^
    $forall widgets w
        ^w^
%div#content
    ^content^
|]



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
    sidebarLayout [] $ do
        h2id <- newIdent
        setTitle $ "Graphmind homepage"
        addWidget $(hamletFile "homepage")



getAnchorR :: Handler RepHtml
getAnchorR = do
  (uid, u) <- requireAuth
  case userAnchor u of
    Nothing  -> notFound
    Just aid -> nodeHandler uid u aid


getViewR :: NodeId -> Handler RepHtml
getViewR nid = do
  (uid, u) <- requireAuth
  -- check the node in question actually belongs to this user
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
  when (nodeOwner node /= uid) $ permissionDenied "You are not the owner of this node."
  links <- runDB $ do
    links <- selectList [LinkFromEq nid] [] 0 0
    justnodes <- mapM (get . linkTo . snd) links
    let nodes = catMaybes $ zipWith (liftA2 (,)) (map (Just . linkTo . snd) links) justnodes
    --nodes <- map (id *** fromJust) . filter (isJust.snd) <$> mapM (\x -> (x, get . linkTo . snd $ x)) links :: YesodDB y (GHandler sub y) [(Key Node, Node)]
    return $ sortBy (comparing (nodeTitle.snd)) nodes -- can't be sorted in the DB given the current implementation of Persistent
  let anchor = fromJust manchor
      linkedToAnchor = (not . null . filter (== aid) . map fst $ links)
  sidebarLayout [anchorWidget aid anchor nid node, actionsWidget aid anchor nid node linkedToAnchor] $ do
    setTitle $ string $ nodeTitle node
    addWidget $(hamletFile "node")



actionsWidget :: NodeId -> Node -> NodeId -> Node -> Bool -> Widget ()
actionsWidget aid anchor nid node linkedToAnchor = do
  let showLinkToAnchor = aid /= nid && not linkedToAnchor
  [$hamlet|
%h3 Actions
%ul
    %li 
        %a!href=@NewR.nid@ Create new node
    %li
        %a!href=@EditR.nid@ Edit this node
    %li 
        %a!href=@DeleteR.nid@ Delete this node
    $if showLinkToAnchor
        %li 
            %form!name="linknodeform"!method=POST!action=@LinkR.nid@
                %a!href="javascript:document.linknodeform.submit()" Link to anchor
    $elseif linkedToAnchor
        %li 
            %form!name="unlinknodeform"!method=POST!action=@UnlinkR.nid@
                %a!href="javascript:document.unlinknodeform.submit()" Unlink from anchor
|]


anchorWidget :: NodeId -> Node -> NodeId -> Node -> Widget ()
anchorWidget aid anchor nid node = do
  let isAnchor = aid == nid
  [$hamlet|
%h3 Anchor
$if isAnchor
    %p Viewing anchor now.
$else
    %b $nodeTitle.anchor$
    %ul
        %li
            %form!name="moveanchorform"!method=POST!action=@MoveAnchorR.nid@
                %a!href="javascript:document.moveanchorform.submit()" Move anchor here
        %li
            %form!name="swapanchorform"!method=POST!action=@SwapR.nid@
                %a!href="javascript:document.swapanchorform.submit()" Swap with current
        %li
            %a!href=@ViewR.aid@ Go to anchor
|]


searchWidget :: Widget ()
searchWidget = [$hamlet|
%h3 Search
%form!method=GET!action=@SearchR@
    %input!type=text!size=20!name=q
    %input!type=submit!value="Search"
%br
%a!href=@OrphansR@ Orphaned nodes
|]


getNewR :: NodeId -> Handler RepHtml
getNewR pid = do
  (uid,u) <- requireAuth
  let heading    = "Create New Node" :: String
      buttontext = "Create" :: String
      action     = [$hamlet|@NewR.pid@|]
  (formcontent,_,nonce) <- generateForm $ editFormlet Nothing
  sidebarLayout [] $ do
    setTitle $ "Create New Node"
    addWidget $(hamletFile "edit_node")

data EditForm = EditForm {
      title :: String
    , body  :: Maybe Textarea
}

editFormlet :: Formlet s m EditForm
editFormlet mparams = fieldsToDivs $ EditForm
    <$> stringField "Node Title" (fmap title mparams)
    <*> maybeTextareaField "Body Text" (fmap body mparams)


postNewR :: NodeId -> Handler ()
postNewR pid = do
  (uid,u) <- requireAuth
  parent <- getNodePerms pid uid
  (res, form, _, _) <- runFormPost $ editFormlet Nothing
  case res of
    FormMissing    -> do { liftIO (putStrLn "FormMissing"); redirect RedirectTemporary $ NewR pid }
    FormFailure s  -> do { liftIO (putStrLn $ "FormFailure" ++ unlines s); redirect RedirectTemporary $ NewR pid } -- FIXME: Needs to preserve the user's partial input and show the errors he made
    FormSuccess ef -> do
      let b_ = fmap unTextarea $ body ef
          b  = case b_ of
                 Nothing -> Nothing
                 Just [] -> Nothing
                 Just x  -> Just (T.pack x)
      nid <- runDB $ do
        nid <- insert (Node (title ef) b uid)
        insert (Link pid nid uid)
        insert (Link nid pid uid)
        return nid
      redirect RedirectTemporary $ ViewR nid


getEditR :: NodeId -> Handler RepHtml
getEditR nid = do
  (uid,u) <- requireAuth
  node <- getNodePerms nid uid
  (formcontent,_,nonce) <- generateForm $ editFormlet $ Just $ EditForm (nodeTitle node) (Textarea . T.unpack <$> nodeBody node)
  let heading    = ("Editing '" ++ nodeTitle node ++ "'") :: String
      buttontext = "Save" :: String
      action     = [$hamlet|@EditR.nid@|]
  sidebarLayout [] $ do
    setTitle $ string $ "Editing '" ++ nodeTitle node ++ "'"
    addWidget $(hamletFile "edit_node")


postEditR :: NodeId -> Handler RepHtml
postEditR nid = do
  (uid,u) <- requireAuth
  node <- getNodePerms nid uid
  (res, form, _, _) <- runFormPost $ editFormlet $ Just $ EditForm (nodeTitle node) (Textarea . T.unpack <$> nodeBody node)
  case res of
    FormMissing    -> redirect RedirectTemporary $ EditR nid
    FormFailure s  -> do { setMessage (string $ unlines s); redirect RedirectTemporary $ EditR nid }
    FormSuccess ef -> do
      let b = case fmap unTextarea $ body ef of
                Nothing -> Nothing
                Just [] -> Nothing
                Just x  -> Just (T.pack x) 
      runDB $ update nid [NodeTitle (title ef), NodeBody b]
      redirect RedirectTemporary $ ViewR nid



getDeleteR :: NodeId -> Handler RepHtml
getDeleteR nid = do
  (uid,u) <- requireAuth
  mnode <- runDB $ get nid
  when (isNothing mnode) notFound
  let node = fromJust mnode
  when (nodeOwner node /= uid) $ permissionDenied "You are not the owner of this node."
  sidebarLayout [] $ do
    setTitle $ "Confirm delete"
    addHamlet $(hamletFile "confirm_delete")


postDeleteR :: NodeId -> Handler ()
postDeleteR nid = do
  (uid,u) <- requireAuth
  runDB $ do
    mnode <- get nid
    when (isNothing mnode) $ lift notFound
    let node = fromJust mnode
    when (nodeOwner node /= uid) $ lift $ permissionDenied "You are not the owner of this node."
    deleteWhere [LinkFromEq nid]
    deleteWhere [LinkToEq   nid]
    delete nid
  redirect RedirectTemporary AnchorR




postLinkR :: NodeId -> Handler ()
postLinkR nid = do
  (uid,u) <- requireAuth
  let aid = maybe (-1) id (userAnchor u)
  runDB $ do
    mnode <- get nid
    when (isNothing mnode) $ lift notFound
    let node = fromJust mnode
    when (nodeOwner node /= uid) $ lift $ permissionDenied "You are not the owner of this node."
    links <- count [LinkFromEq nid, LinkToEq aid]
    if links > 0 then return () else do
        insert (Link aid nid uid)
        insert (Link nid aid uid)
        return ()
  redirect RedirectTemporary $ ViewR nid


postUnlinkR :: NodeId -> Handler ()
postUnlinkR nid = do
  (uid, u) <- requireAuth
  let aid = maybe (-1) id (userAnchor u)
  runDB $ do
    mnode <- get nid
    when (isNothing mnode) $ lift notFound
    let node = fromJust mnode
    when (nodeOwner node /= uid) $ lift $ permissionDenied "You are not the owner of this node."
    deleteWhere [LinkFromEq aid, LinkToEq nid]
    deleteWhere [LinkFromEq nid, LinkToEq aid]
  redirect RedirectTemporary $ ViewR nid




postMoveAnchorR :: NodeId -> Handler ()
postMoveAnchorR nid = do
  (uid, u) <- requireAuth
  node <- getNode nid
  when (nodeOwner node /= uid) $ permissionDenied "You are not the owner of this node."
  runDB $ update uid [UserAnchor $ Just nid]
  redirect RedirectTemporary $ ViewR nid


postSwapR :: NodeId -> Handler ()
postSwapR nid = do
  (uid, u) <- requireAuth
  node <- getNode nid
  when (nodeOwner node /= uid) $ permissionDenied "You are not the owner of this node."
  let oldAnchor = maybe nid id (userAnchor u)
  runDB $ update uid [UserAnchor $ Just nid]
  redirect RedirectTemporary $ ViewR oldAnchor



-- TODO: This currently gets only nodes without neighbours. a connected but detached subgraph is currently only locatable via search
getOrphansR :: Handler RepHtml
getOrphansR = do
  (uid,u) <- requireAuth
  orphans <- runDB $ do
    nodes   <- selectList [NodeOwnerEq uid] [NodeTitleAsc] 0 0
    filterM (\(nid,_) -> count [LinkFromEq nid] >>= \n -> return (n == 0)) nodes
  let noOrphans = null orphans
  sidebarLayout [] $ do
    setTitle "Orphaned Nodes"
    addWidget $(hamletFile "orphans")
  

getSearchR :: Handler RepHtml
getSearchR = do
  (uid,u) <- requireAuth
  mquery <- lookupGetParam "q"
  let query = maybe "" (map toLower) mquery
  case query of
    [] -> setMessage (string "Please enter a search string.") >> defaultLayout (do { setTitle "Search"; addWidget searchWidget })
    _  -> do
      nodes <- runDB $ selectList [NodeOwnerEq uid] [NodeTitleAsc] 0 0
      let hits = filter (\(_,n) -> query `isInfixOf` map toLower (nodeTitle n) || query `isInfixOf` maybe "" (map toLower . T.unpack) (nodeBody n)) nodes
      sidebarLayout [] $ do
        setTitle "Search Results"
        addWidget $(hamletFile "search_results")
  


