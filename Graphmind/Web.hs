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
) where

import Graphmind.Types
import Graphmind.Backend
import Graphmind.Sessions

import Database.HDBC
import Database.HDBC.Sqlite3

import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B

import Network.FastCGI
import Text.XHtml
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

pg :: Html -> CGI CGIResult
pg = output . showHtml


s2h = stringToHtml


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


preMap :: M.Map String Pre
preMap = M.fromList []


-- | Special, like 'pgLogin'.
preLogin :: Connection -> CGI CGIResult
preLogin c = do
  username <- getInput "gmUser"
  password <- getInput "gmPwd"

  case (username,password) of
    (Just u, Just p) -> do
      let hash = showDigest . sha1 . B.pack $ p
      rs <- liftIO $ quickQuery' c "SELECT _id FROM User WHERE username = ?, password = ?" [toSql u, toSql hash]
      case rs of
        [[uid]] -> do
          newSession c $ fromSql uid
          redirect "/graphmind?pg=View"
        _       -> do
          setError "The username or password provided is incorrect. Please try again."
          redirect "/graphmind?pg=Login"
    _ -> do
      setError "You must provide a username and password."
      redirect "/graphmind?pg=Login"



-----------------------------------------------------------------------------
-- pg handlers
-----------------------------------------------------------------------------

pgMap :: M.Map String Pg
pgMap = M.fromList []


pgView :: Pg
pgView = undefined


-- | Display the login page.
--
-- This pg is special, it's not in GM since there's no UserId yet.

pgLogin :: CGI CGIResult
pgLogin = pg =<< (pgTemplate "Graphmind Login" $ 
  form ! [action "/graphmind?pre=Login&pg=View", method "POST"]
    << table << tbody
      << (tr << (td << s2h "Username:" +++ td << s2h "Password:")
      +++ tr << (td << textfield "" ! [size "30", maxlength 50, name "gmUser"]
             +++ td << password ""  ! [size "30", maxlength 50, name "gmPwd"])
      )
    +++ submit "Log In" "login")

