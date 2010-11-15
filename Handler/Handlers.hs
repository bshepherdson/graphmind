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
  defaultLayout $ do
    setTitle $ string $ nodeTitle node
    addWidget $(hamletFile "node")
    let anchor = fromJust manchor
    addWidget $ actionsWidget aid anchor nid node (not . null . filter (== aid) . map fst $ links)
    addWidget $ anchorWidget aid anchor nid node
    addWidget $ searchWidget



actionsWidget :: NodeId -> Node -> NodeId -> Node -> Bool -> Widget ()
actionsWidget aid anchor nid node linkedToAnchor = do
  let showLinkToAnchor = aid /= nid && not linkedToAnchor
  [$hamlet|
%h3 Actions
%ul
    %li 
        %a!href=@NewR.nid@ Create new node
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
anchorWidget aid anchor nid node = [$hamlet|
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



getNewR :: NodeId -> Handler RepHtml
getNewR pid = do
  (uid,u) <- requireAuth
  let heading = "Create New Node" :: String
  (formcontent,_,nonce) <- generateForm $ editFormlet Nothing
  defaultLayout $ do
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
  mparent <- runDB $ get pid
  when (isNothing mparent) $ notFound
  let parent = fromJust mparent
  when (nodeOwner parent /= uid) $ permissionDenied "You do not own the parent node."
  liftIO $ putStrLn $ "postNewR: " ++ show (fromPersistKey pid)
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


getDeleteR :: NodeId -> Handler RepHtml
getDeleteR nid = do
  (uid,u) <- requireAuth
  mnode <- runDB $ get nid
  when (isNothing mnode) notFound
  let node = fromJust mnode
  when (nodeOwner node /= uid) $ permissionDenied "You are not the owner of this node."
  defaultLayout $ do
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

