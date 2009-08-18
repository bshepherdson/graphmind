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

import Network.FastCGI
import Text.XHtml hiding (title,text,target)
import Data.Digest.Pure.SHA


-----------------------------------------------------------------------------
-- utility functions
-----------------------------------------------------------------------------

-- | Takes the title and body and constructs the page.
--
-- Needs to be in CGI because it uses the @err@ parameter.
pgTemplate :: String -> Html -> CGI Html
pgTemplate t b = do
  err <- getError
  return $ header << thetitle << s2h t +++ body << (
             h1 << t +++ err +++ b
           )

showPg :: Html -> CGI CGIResult
showPg = output . showHtml


pg :: CGI Html -> CGI CGIResult
pg h = h >>= showPg


s2h :: String -> Html
s2h = stringToHtml


nodeLinks :: Node -> [HotLink]
nodeLinks n = map (\(i,t) -> hotlink (target $ "pg=View&view=" ++ show i) (s2h t)) (adjacent n)

target :: String -> String
target s = "/graphmind.fcgi?" ++ s


-----------------------------------------------------------------------------
-- error handling
-----------------------------------------------------------------------------

setError :: String -> CGI ()
setError = setCookie . newCookie "graphmind-error"


-- either prints the error message or nothing
getError :: CGI Html
getError = do
  c <- getCookie "graphmind-error"
  case c of
    Nothing -> return noHtml
    Just e  -> do
      deleteCookie $ newCookie "graphmind-error" ""
      return $ paragraph ! [theclass "error"] << s2h e


-----------------------------------------------------------------------------
-- pre handlers
-----------------------------------------------------------------------------


-- deliberately does not include preLogin
preMap :: M.Map String Pre
preMap = M.fromList []


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
          setError "The username or password provided is incorrect. Please try again."
          --redirect (target "pg=Login")
          return Nothing
    _ -> do
      setError "You must provide a username and password."
      --redirect (target "pg=Login")
      return Nothing



-----------------------------------------------------------------------------
-- pg handlers
-----------------------------------------------------------------------------

--type Pg = GM CGIResult
-- deliberately does not include pgLogin
pgMap :: M.Map String Pg
pgMap = M.fromList [("View",pgView)]


pgView :: Pg
pgView = do
  (GMState _ _) <- ask
  a <- getAnchor
  i <- cgi $ readInput "view"
  v <- case i of 
         Just n  -> getNode n >>= \n' -> case n' of
                      Nothing   -> cgi (setError $ "Node " ++ show n ++ " not found.") >> getAnchor
                      Just node -> return node
         Nothing -> getAnchor
  cgi $ pg $ pgTemplate (title v) $
    h3 << s2h "Links"
    +++ unordList (nodeLinks v)
    +++ case text v of
          Nothing -> noHtml
          Just t  -> h3 << s2h "text" +++ paragraph << s2h t
    +++ anchorWidget a v
  
 where anchorWidget a n 
         | a == n    = h3 << s2h "Anchor" +++ paragraph << s2h "Viewing anchor now."
         | otherwise = h3 << s2h "Anchor" +++ paragraph << bold << s2h (title a) +++ unordList [
                   hotlink (target $ "pre=MoveAnchor&pg=View&anchor=" ++ show (_id a) ++ "&view=" ++ show (_id n)) (s2h "Move anchor here")
                  ,hotlink (target $ "pre=Swap&pg=View&view=" ++ show (id n)) (s2h "Swap")
                  ]



-- | Display the login page.
--
-- This pg is special, it's not in GM since there's no UserId yet.

pgLogin :: CGI CGIResult
pgLogin = pg $ pgTemplate "Graphmind Login" $ 
  form ! [action $ target "pre=Login&pg=View", method "POST"]
    << (table << tbody
      << (tr << (td << s2h "Username:" +++ td << textfield "" ! [size "30", maxlength 50, name "gmUser"])
      +++ tr << (td << s2h "Password:" +++ td << password ""  ! [size "30", maxlength 50, name "gmPwd"])
      )
    +++ submit "login" "Log In")

