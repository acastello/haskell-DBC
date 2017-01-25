{-# LANGUAGE OverloadedStrings #-}

import Codec.Compression.GZip

import Data.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Scientific
import Data.Serialize
import Data.Text
import Data.Text.Encoding
import Data.Time

import Database.MySQL.Base

import qualified System.IO.Streams as S

import Types

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

saveResult :: FilePath -> [[MySQLValue]] -> IO ()
saveResult file res = BL.writeFile file $ compress $ encodeLazy res

loadResult :: FilePath -> IO [[MySQLValue]]
loadResult file = either error id . decodeLazy . decompress <$> BL.readFile file

getRes :: [MySQLValue] -> Maybe [(Stat, Int)]
getRes entry = do
    undefined

just n = if n/=0 then Just n else Nothing
