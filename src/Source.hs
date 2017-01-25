{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString
import qualified Data.ByteString as B
import Data.Scientific
import Data.Serialize
import Data.Text
import Data.Text.Encoding
import Data.Time

import Database.MySQL.Base

import qualified System.IO.Streams as S

queryString :: Query
queryString = "SELECT ID, MINLEVEL FROM `quest_template` WHERE ? IN (`RewardChoiceItemID1`\
             \, `RewardChoiceItemID2`, `RewardChoiceItemID3`, `RewardChoiceItemID4`)"

q id = do
    conn <- connect defaultConnectInfo { ciUser = "guest"
        , ciHost = "192.168.1.111", ciDatabase = "world" }
    s <- prepareStmt conn queryString
    print =<< S.read . snd =<< queryStmt conn s [MySQLInt32U id]
    close conn
    return ()

instance Serialize Text where
    put txt = put $ encodeUtf8 txt
    get     = fmap decodeUtf8 get

instance Serialize Day where
    put = undefined
    get = undefined

instance Serialize TimeOfDay where
    put = undefined
    get = undefined

instance Serialize LocalTime where
    put = undefined
    get = undefined

instance Serialize Scientific where
    put = undefined
    get = undefined

instance Serialize MySQLValue
