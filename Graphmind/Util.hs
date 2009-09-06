----------------------------------------------------------------------------
-- |
-- Module      :  Graphmind.Util
-- Copyright   :  (c) Braden Shepherdson 2009
-- License     :  BSD3
--
-- Maintainer  :  Braden.Shepherdson@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions used in common by Web, Help and other modules.
--
-----------------------------------------------------------------------------

module Graphmind.Util where

import Graphmind.Types

import Network.FastCGI
import Text.XHtml hiding (title,text,target,pre,content)

import Data.Time
import Data.List (intercalate)
import qualified Data.Map as M

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
nodeLinks = listLinks . adjacent

listLinks :: [(NodeId,String)] -> [HotLink]
listLinks xs = map (\(i,t) -> gmlink "" "View" [("view", show i)] (s2h t)) xs

target :: String -> String
target s = "/graphmind/graphmind.fcgi?" ++ s

-- pre, pg, other params, link text
gmlink :: String -> String -> [(String,String)] -> Html -> HotLink
gmlink pre pg' params content = hotlink (target . intercalate "&" . map (\(x,y) -> x++"="++y) $ params') content
  where params' | null pre  = ("pg",pg') : params
                | otherwise = [("pre", pre), ("pg", pg')] ++ params







-- helper functions

gmInput :: String -> GM (Maybe String)
gmInput s = do
  v <- M.lookup s <$> gets vars
  c <- cgi $ getInput s
  return $ v `mplus` c
  
gmSetInput :: String -> String -> GM ()
gmSetInput k v = modify $ \st -> st { vars = M.insert k v (vars st) }

gmReadInput :: (Read a) => String -> GM (Maybe a)
gmReadInput s = fmap read <$> gmInput s


-- debugging

logmsg :: String -> IO ()
logmsg s = do
  t <- getCurrentTime
  appendFile "/var/log/graphmind.log" $ "[" ++ show t ++ "] " ++ s ++ "\n"

gmRedirect :: String -> CGI CGIResult
gmRedirect t = setHeader "Location" t >> outputNothing




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

