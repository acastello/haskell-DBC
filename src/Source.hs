{-# LANGUAGE OverloadedStrings #-}

module Source where

import Codec.Compression.GZip

import qualified Data.Array as A
import Data.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import Data.Maybe
import Data.Scientific
import Data.Serialize
import Data.Text
import Data.Text.Encoding
import Data.Time
import qualified Data.Vector as V
import Data.Word

import Database.MySQL.Base

import qualified System.IO.Streams as S

import Types
import DBC

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

save :: Serialize a => FilePath -> a -> IO ()
save file res = BL.writeFile file $ compress $ encodeLazy res

load :: Serialize a => FilePath -> IO a
load file = either error id . decodeLazy . decompress <$> BL.readFile file

saveResult :: FilePath -> [[MySQLValue]] -> IO ()
saveResult file res = BL.writeFile file $ compress $ encodeLazy res

loadResult :: FilePath -> IO [[MySQLValue]]
loadResult file = either error id . decodeLazy . decompress <$> BL.readFile file

loadSpells :: IO [Spell]
loadSpells = load "spells.gz"

saveSpells :: [Spell] -> IO ()
saveSpells sp = save "spells.gz" sp

loadItems :: IO [Item]
loadItems = load "items.gz"

saveItems :: [Item] -> IO ()
saveItems is = save "items.gz" is

getRes :: [MySQLValue] -> [(Stat, Int)]
getRes entry = 
    catMaybes $ L.zipWith just' 
        [HolyRes, FireRes, NatureRes, FrostRes, ShadowRes, ArcaneRes]
        (fs <$> inds entry [57..62])

getMainStats :: [MySQLValue] -> [(Stat, Int)]
getMainStats entry = catMaybes $ do
    i <- [0..n-1]
    return $ just' (toEnum $ fs $ entry!!(28+2*i)) (fs $ entry!!(29+2*i))
    where n = fs (entry!!27)

getSpells :: [MySQLValue] -> [Word32]
getSpells e = do
    i <- fs <$> inds e [66,73,80,87,94]
    if i /= 0 then
        return i
    else
        []
                    
getItem :: [Spell] -> [MySQLValue] -> Item
getItem sp e =
    let iid = fs (e!!0)
        iname = (\(MySQLText t) -> encodeUtf8 t) (e!!4)
        islot = toEnum $ fs (e!!12)
        istats = (getMainStats e) ++ (getRes e)
        ilevel = fs (e!!15)
        ispells = getSpells e
        ispells' = foldMap (\s -> L.filter (\s' -> s == sid s') sp) ispells
        istats' = catMaybes $ getSpellStats <$> ispells'
    in Item iid iname islot (istats++istats') ilevel ispells noRequirements 
        (foldMap (B.append "\n") (sdesc <$> ispells'))

just :: (Eq a, Num a) => a -> Maybe a
just n = if n/=0 then Just n else Nothing

just' :: (Eq b, Num b) => a -> b -> Maybe (a,b)
just' a b = (,) a <$> just b

inds :: [a] -> [Int] -> [a]
inds entry indices = (!!) <$> pure entry <*> indices

fs :: Num a => MySQLValue -> a
fs i = case i of
    MySQLInt8 n -> fromIntegral n
    MySQLInt8U n -> fromIntegral n
    MySQLInt16 n -> fromIntegral n
    MySQLInt16U n -> fromIntegral n
    MySQLInt32 n -> fromIntegral n
    MySQLInt32U n -> fromIntegral n
    MySQLInt64 n -> fromIntegral n
    MySQLInt64U n -> fromIntegral n
