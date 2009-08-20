{-# LANGUAGE ExistentialQuantification #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Graphmind.Web
-- Copyright   :  (c) Braden Shepherdson 2009
-- License     :  BSD3
--
-- Maintainer  :  Braden.Shepherdson@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Core of the web interface, all the pre and pg handler functions.
--
-----------------------------------------------------------------------------

module Graphmind.Web (
   preMap
  ,preLogin

  ,pgMap
  ,pgView
  ,pgLogin

  ,target
) where

import Graphmind.Types
import Graphmind.Backend
import Graphmind.Sessions

import Database.HDBC
import Database.HDBC.Sqlite3

import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (intercalate)
import Data.Maybe (isJust,fromMaybe)

import Network.FastCGI
import Text.XHtml hiding (title,text,target,pre,content)
import Data.Digest.Pure.SHA


-----------------------------------------------------------------------------
-- utility functions
-----------------------------------------------------------------------------

-- | Takes the title and body and constructs the page.
--
-- Needs to be in CGI because it uses the @err@ parameter.
pgTemplate :: String -> Html -> GM Html
pgTemplate t b = do
  err  <- cgi $ getErrorNextReq
  err' <- getErrorThisReq
  return $ header << thetitle << s2h t +++ body << (
             h1 << t +++ err +++ err' +++ b
           )

cgiPgTemplate :: String -> Html -> CGI Html
cgiPgTemplate t b = do
  err  <- getErrorNextReq
  return $ header << thetitle << s2h t +++ body << (
             h1 << t +++ err +++ b
           )


showPg :: Html -> CGI CGIResult
showPg = output . showHtml


pg :: GM Html -> GM CGIResult
pg h = h >>= cgi . showPg


cgiPg :: CGI Html -> CGI CGIResult
cgiPg h = h >>= showPg


s2h :: String -> Html
s2h = stringToHtml


nodeLinks :: Node -> [HotLink]
nodeLinks n = map (\(i,t) -> hotlink (target $ "pg=View&view=" ++ show i) (s2h t)) (adjacent n)

target :: String -> String
target s = "/graphmind.fcgi?" ++ s

-- pre, pg, other params, link text
gmlink :: String -> String -> [(String,String)] -> Html -> HotLink
gmlink pre pg' params content = hotlink (target . intercalate "&" . map (\(x,y) -> x++"="++y) $ params') content
  where params' | null pre  = ("pg",pg') : params
                | otherwise = [("pre", pre), ("pg", pg')] ++ params


getView :: GM Node
getView = getNodeFromParamAnchor "view"

getNodeFromParamAnchor :: String -> GM Node
getNodeFromParamAnchor s = do
  i <- getNodeIdFromParam s
  case i of 
    Just n  -> getNode n >>= \n' -> case n' of
                 Nothing   -> (setErrorThisReq $ "Node " ++ show n ++ " not found.") >> getAnchor
                 Just node -> return node
    Nothing -> getAnchor


getNodeIdFromParamAnchor :: String -> GM NodeId
getNodeIdFromParamAnchor s = do
  m <- getNodeIdFromParam s
  case m of 
    Just n  -> return n
    Nothing -> _id <$> getAnchor


getNodeIdFromParam :: String -> GM (Maybe NodeId)
getNodeIdFromParam s = fmap read <$> gmInput s -- two layers to fmap into


-----------------------------------------------------------------------------
-- error handling
-----------------------------------------------------------------------------

setErrorNextReq :: String -> CGI ()
setErrorNextReq = setCookie . newCookie "graphmind-error"


-- either prints the error message or nothing
getErrorNextReq :: CGI Html
getErrorNextReq = do
  c <- getCookie "graphmind-error"
  case c of
    Nothing -> return noHtml
    Just e  -> do
      deleteCookie $ newCookie "graphmind-error" ""
      return $ paragraph ! [theclass "error"] << s2h e


setErrorThisReq :: String -> GM ()
setErrorThisReq s = modify $ \st -> st { errors = errors st ++ [s] }

getErrorThisReq :: GM Html
getErrorThisReq = do
  es <- gets errors
  modify $ \st -> st { errors = [] }
  return $ paragraph ! [theclass "error"] << unordList (map s2h es)

-----------------------------------------------------------------------------
-- pre handlers
-----------------------------------------------------------------------------


-- deliberately does not include preLogin
preMap :: M.Map String Pre
preMap = M.fromList [
           ("New",preNew)
          ,("Delete",preDelete)
          ,("MoveAnchor",preMoveAnchor)
          ]



-- | Creates a new node. The node has a link to the node given in the @parent@ attribute, defaulting to
-- the anchor.
preNew :: Pre
preNew = do
  parent <- getNodeIdFromParamAnchor "parent"
  fTitle <- fromMaybe "Untitled Node" <$> gmInput "title"
  fText  <- gmInput "text" >>= \m -> case m of
               Nothing -> return Nothing
               Just "" -> return Nothing
               Just tx -> return (Just tx)
  let n = Node { _id = 0, title = fTitle, text = fText, adjacent = [(parent, "")] }
  nid <- createNode n
  gmSetInput "pg"     "View"
  gmSetInput "view" $ show nid


preDelete :: Pre
preDelete = do
  mdelete <- getNodeIdFromParam "delete"
  case mdelete of
    Nothing -> return ()
    Just d  -> deleteNodeById d -- let View default to the anchor.


preMoveAnchor :: Pre
preMoveAnchor = do
  ma <- gmReadInput "anchor"
  case ma of
    Nothing -> return ()
    Just a  -> io (logmsg $ "Setting anchor to " ++ show a) >> setAnchor a >> io (logmsg $ "Done setting anchor")




-- | Special, like 'pgLogin'.
preLogin :: Connection -> CGI (Maybe UserId)
preLogin c = do
  username <- getInput "gmUser"
  pswd <- getInput "gmPwd"

  liftIO $ logmsg $ "Running preLogin..."
  case (username,pswd) of
    (Just u, Just pw) -> do
      liftIO $ logmsg $ "Got user '" ++ u ++ "' and password '" ++ pw ++ "'."
      let hash = showDigest . sha1 . B.pack $ pw
      liftIO $ logmsg $ "Got hash of '" ++ hash ++ "'."
      rs <- liftIO $ quickQuery' c "SELECT _id FROM User WHERE username = ? AND password = ?" [toSql u, toSql hash]
      case rs of
        [[uid]] -> do
          liftIO $ logmsg $ "Found userid " ++ show (fromSql uid :: Int) ++ "."
          newSession c $ fromSql uid
          liftIO $ commit c
          --gmRedirect (target "pg=View")
          return . Just . fromSql $ uid
        _       -> do
          setErrorNextReq "The username or password provided is incorrect. Please try again."
          --redirect (target "pg=Login")
          return Nothing
    _ -> do
      setErrorNextReq "You must provide a username and password."
      --redirect (target "pg=Login")
      return Nothing


-----------------------------------------------------------------------------
-- pg handlers
-----------------------------------------------------------------------------

--type Pg = GM CGIResult
-- deliberately does not include pgLogin
pgMap :: M.Map String Pg
pgMap = M.fromList [("View",pgView),("New", pgNew),("Delete", pgDelete)]


pgView :: Pg
pgView = do
  a <- getAnchor
  v <- getView
  pg $ pgTemplate (title v) $
    h3 << s2h "Links"
    +++ unordList (nodeLinks v)
    +++ case text v of
          Nothing -> noHtml
          Just t  -> h3 << s2h "Text" +++ paragraph << s2h t
    +++ actionWidget a v
    +++ anchorWidget a v
  

pgNew :: Pg
pgNew = do
  v <- getNodeIdFromParamAnchor "parent"
  pg . pgTemplate "New Node" $
    form ! [action . target $ "pre=New&parent=" ++ show v, method "POST"]
    << (paragraph << bold << s2h "Title"
    +++ textfield "" ! [size "50", maxlength 255, name "title"]
    +++ paragraph << bold << s2h "Text"
    +++ textarea ! [rows "10", cols "50", name "text"] << noHtml
    +++ br +++ br
    +++ submit "create" "Create Node")
    

pgDelete :: Pg
pgDelete = do
  n <- getNodeFromParamAnchor "delete"
  pg . pgTemplate "Confirm Deletion" $
    paragraph << (
      s2h "Are you certain you want to delete the node '"
      +++ bold << s2h (title n)
      +++ "'?"
      +++ unordList [
             gmlink "Delete" "View" [("delete", show (_id n))] (s2h "Yes, delete the node") -- defaults to viewing the anchor.
            ,gmlink ""       "View" [("view",   show (_id n))] (s2h "No, go back.")
            ]
    )


-- | Display the login page.
--
-- This pg is special, it's not in GM since there's no UserId yet.

pgLogin :: CGI CGIResult
pgLogin = cgiPg $ cgiPgTemplate "Graphmind Login" $ 
  form ! [action $ target "pre=Login&pg=View", method "POST"]
    << (table << tbody
      << (tr << (td << s2h "Username:" +++ td << textfield "" ! [size "30", maxlength 50, name "gmUser"])
      +++ tr << (td << s2h "Password:" +++ td << password ""  ! [size "30", maxlength 50, name "gmPwd"])
      )
    +++ submit "login" "Log In")




-----------------------------------------------------------------------------
-- widgets
-----------------------------------------------------------------------------

anchorWidget :: Node -> Node -> Html
anchorWidget a n 
         | a == n    = h3 << s2h "Anchor" +++ paragraph << s2h "Viewing anchor now."
         | otherwise = h3 << s2h "Anchor" +++ paragraph << bold << s2h (title a) +++ unordList [
                   gmlink "MoveAnchor" "View" [("anchor", show (_id n)), ("view", show (_id n))] (s2h "Move anchor here")
                  ,gmlink "Swap" "View" [("view", show (_id n))] (s2h "Swap with current")
                  ,gmlink "" "View" [("view", show (_id a))] (s2h "Go to anchor")
                  ]

actionWidget :: Node -> Node -> Html
actionWidget a n = h3 << s2h "Actions" +++ unordList (map snd . filter fst $ [
     (True, gmlink "" "New" [("parent", show (_id n))] (s2h "Create new node"))
    ,(True, gmlink "" "Delete" [("delete", show (_id n))] (s2h "Delete this node"))
    ,(a /= n && not neighbours, 
        gmlink "Link" "View" [("link1", show (_id a)), ("link2", show (_id n)), ("view", show (_id n))] (s2h "Link to anchor"))
    ,(neighbours, gmlink "Unlink" "View" [("link1", show (_id a)), ("link2", show (_id n)), ("view", show (_id n))] (s2h "Unlink from anchor"))
    ])
  where neighbours = isJust . lookup (_id n) . adjacent $ a



