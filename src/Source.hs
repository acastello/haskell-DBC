{-# LANGUAGE OverloadedStrings #-}

module Source where

import Codec.Compression.GZip

import Control.Exception (evaluate)

import qualified Data.Array as A
import Data.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap as M
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


q tab = do
    conn <- connect defaultConnectInfo { ciUser = "guest"
        , ciHost = "192.168.1.111", ciDatabase = "world" }
    ret <- S.toList . snd =<< query_ conn tab
    close conn
    return ret

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

saveComp :: Serialize a => FilePath -> [String] -> String -> a -> IO ()
saveComp file imps var e = let varname = (L.head $ L.words var) in Prelude.writeFile (file ++ ".hs") $
                          "module " ++ file ++ " where\n" ++
                          "import Data.Serialize\n" ++
                          "import Codec.Compression.GZip\n\n" ++
                          (foldMap (\s -> s ++ "\n") imps) ++ "\n" ++
                          (if "::" `L.isInfixOf` var then var else "") ++ "\n" ++
                          "" ++ varname ++ " = either error id $ decodeLazy $ decompress $ "
                          ++ (show $ compress $ encodeLazy e) ++ "\n" ++
                          "{-# INLINE " ++ varname ++ " #-}"

saveCompItems :: M.IntMap Item -> IO ()
saveCompItems = saveComp "Raw_items" ["import Data.IntMap", "import Types"] "raw_items :: IntMap Item"

saveResult :: FilePath -> [[MySQLValue]] -> IO ()
saveResult file res = BL.writeFile file $ compress $ encodeLazy res

loadResult :: FilePath -> IO [[MySQLValue]]
loadResult file = either error id . decodeLazy . decompress <$> BL.readFile file

loadQuests :: IO (M.IntMap Quest)
loadQuests = load "quests.gz"

saveQuests :: M.IntMap Quest -> IO ()
saveQuests = save "quests.gz"

loadSpells :: IO (M.IntMap Spell)
loadSpells = load "spells.gz"

saveSpells :: M.IntMap Spell -> IO ()
saveSpells sp = save "spells.gz" sp

loadItems :: IO (M.IntMap Item)
loadItems = load "items.gz"

saveItems :: M.IntMap Item -> IO ()
saveItems is = save "items.gz" is


getQuest :: [MySQLValue] -> Quest
getQuest e =
    let qid = fs (e!!0)
        qname = (\(MySQLText t) -> encodeUtf8 t) (e!!74)
        rmask = fs (e!!73)
        r_fac = case rmask of 0x2b2 -> Just Horde
                              0x44d -> Just Alliance
                              _ -> Nothing
        r_level = fs (e!!3)
        qitems = L.filter (/=0) $ fs <$> inds e [22,24,26,28,38,40,42,44,46,48]
    in Quest qid qname r_level r_fac qitems

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

getSpells :: [MySQLValue] -> [Int]
getSpells e = do
    i <- fs <$> inds e [66,73,80,87,94]
    if i /= 0 then
        return i
    else
        []
                    
getItem :: M.IntMap Spell -> M.IntMap Quest -> [MySQLValue] -> Item
getItem sp qs e =
    let iid = fs (e!!0)
        iname = (\(MySQLText t) -> encodeUtf8 t) (e!!4)
        iqual = toEnum $ fs (e!!6)
        islot = toEnum $ fs (e!!12)
        iatype = toEnum $ fs (e!!108)
        istats = (getMainStats e) ++ (getRes e)
        ilevel = fs (e!!15)
        rlevel = fs (e!!16)
        iqs = M.filter (\q -> L.any (== iid) (qitems q)) qs
        reqlevel = if M.null iqs then 
                      rlevel 
                    else 
                      max rlevel (qlevel $ snd $ L.head $ M.toList iqs)
        reqlevel' = if iqual > Uncommon && reqlevel == 0 then -1 else reqlevel
        ispells = getSpells e
        ispells' = catMaybes $ flip M.lookup sp <$> ispells -- foldMap (\s -> L.filter (\s' -> s == sid s') sp) ispells
        istats' = catMaybes $ getSpellStats <$> ispells'
    in Item iid iname islot iatype (istats++istats') ilevel iqual reqlevel'
        (foldMap (B.append "\n") (sdesc <$> ispells'))

getItems :: IO (M.IntMap Item)
getItems = do
    irs <- evaluate =<< loadResult "item_template.gz"
    ss <- evaluate =<< loadSpells
    qs <- evaluate =<< loadQuests
    evaluate $ M.fromList $ fmap (\x -> (iid x, x)) $ getItem ss qs <$> irs


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
