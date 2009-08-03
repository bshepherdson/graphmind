module Graphmind.Sessions (
     checkSession
    ,newSession
    ,deleteSession
) where


import Network.FastCGI
 
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B
import System.Random

import Graphmind.Types

import Database.HDBC
import Database.HDBC.Sqlite3


-- | Returns Just the UserId if the session is valid, and Nothing if not.
checkSession :: Connection -> CGI (Maybe UserId)
checkSession c = do
  cookie <- getCookie "graphmind_session"
  rs     <- liftIO $ quickQuery' c "SELECT user FROM Session WHERE hash = ?" [toSql cookie]
  case rs of
    [[u]] -> return $ Just $ fromSql u
    _   -> return Nothing


-- | Can be used after login or registration.
newSession :: Connection -> UserId -> CGI ()
newSession c u = do
  r <- liftIO randomIO :: CGI Int
  let hash = showDigest . sha1 . B.pack $ show u ++ "__" ++ show r
  liftIO $ run c "INSERT INTO Session (user,hash) VALUES (?,?)" [toSql u, toSql hash]
  setCookie $ (newCookie "graphmind_session" hash) { cookiePath = Nothing }


-- | Delete the named user's session.
deleteSession :: Connection -> UserId -> CGI ()
deleteSession c u = do
  liftIO $ run c "DELETE FROM Session WHERE user = ?" [toSql u]
  return ()

