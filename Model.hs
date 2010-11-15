{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Model where

import Yesod
import Database.Persist.TH (share2)
import Database.Persist.GenericSql (mkMigrate)

import Data.Text
import Data.ByteString

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share2 mkPersist (mkMigrate "migrateAll") [$persist|
User
    ident String
    password String null update
    anchor NodeId null update
    UniqueUser ident
Email
    email String
    user UserId null update
    verkey String null update
    UniqueEmail email

Node
    title String update In Asc
    body Text null update In
    owner UserId Eq
    UniqueNode title owner

Link
    from  NodeId Eq
    to    NodeId Eq
    owner UserId Eq

    
|]
