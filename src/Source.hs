{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell,
 FlexibleInstances #-}

module Source where

import Codec.Compression.GZip

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad

import qualified Data.Array as A
import Data.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.IntMap as M
import Data.IntMap (IntMap)
import qualified Data.List as L
import qualified Data.Map as Ma
import Data.Maybe
import Data.Scientific
import Data.Serialize
import Data.Text
import Data.Text.Encoding
import Data.Time
import Data.Time.Calendar
import qualified Data.Vector as V
import Data.Word

import Database.MySQL.Base

import GHC.Generics

import qualified System.IO.Streams as S

import Core
import CompileTime

data DBCSources = DBCSources
    { dbcs_spells     :: IntMap Spell
    , dbcs_suffixes   :: IntMap DBCSuffix
    , dbcs_properties :: IntMap DBCProperty
    , dbcs_enchants   :: IntMap DBCEnchantment
    } deriving Generic
instance Serialize DBCSources

instance Make DBCSources where
    file = undefined
    make = liftM4 DBCSources make make make make
    load' = liftM4 DBCSources load' load' load' load'
    save' (DBCSources a b c d) = save' a >> save' b >> save' c >> save' d

data SQLSources = SQLSources
    { sqls_items      :: IntMap SQLItem
    , sqls_suffmap    :: IntMap SQLSuffix
    , sqls_disench    :: IntMap SQLDisenchant
    , sqls_gobjs      :: IntMap GameObject
    } deriving Generic
instance Serialize SQLSources

instance Make SQLSources where
    file _ = undefined
    make = liftM4 SQLSources make make make make
    load' = liftM4 SQLSources load' load' load' load'
    save' (SQLSources a b c d) = save' a >> save' b >> save' c >> save' d

data Maps = Maps
    { map_suffmap     :: SuffixMap
    } deriving Generic
instance Serialize Maps

instance Make Maps where
    file = undefined
    make = liftM Maps make
    load' = liftM Maps load'
    save' (Maps a) = save' a 

data FinalData = FinalData
    { final_items     :: IntMap Item
    } deriving Generic
instance Serialize FinalData

instance Make FinalData where
    file = undefined
    make = liftM FinalData make
    load' = liftM FinalData load'
    save' (FinalData a) = save' a


instance Make SuffixMap where
    file _ = "suffixMap.gz"
    make = return $ makeSuffixMap dbcSources sqlSources

makeSuffixMap :: DBCSources -> SQLSources -> SuffixMap
makeSuffixMap dbc sql = Ma.fromList (f $ M.elems (sqls_suffmap sql)) where
    f = foldMap $ \(SQLSuffix id xs) -> [(Left id, leftmap xs), (Right id, rightmap xs)]
    map sourcef enchsf stringf xs = do
        (id, chance) <- xs
        L.reverse $ L.sortOn su_chance $ groupSuffixes $ maybeToList $ do
            sid <- M.lookup id (sourcef dbc)
            encs <- mapM (flip M.lookup $ dbcs_enchants dbc) (enchsf sid)
            return $ getSuffix
                (dbcs_spells dbc) (stringf sid) chance 
                (encs >>= dbcen_stats) (encs >>= dbcen_spells) 
    leftmap = map dbcs_suffixes dbcsu_enchs dbcsu_suffix
    rightmap = map dbcs_properties dbcpo_enchs dbcpo_suffix

instance Make (IntMap Item) where
    file _ = "items.gz"
    make = return $ fmap (makeItem dbcSources sqlSources maps) 
           $ (sqls_items sqlSources)

makeItem :: DBCSources -> SQLSources -> Maps -> SQLItem -> Item
makeItem dbc sql maps it = Item
    (sqlit_id it) (sqlit_name it) desc (sqlit_level it) (sqlit_qual it)
    (sqlit_mat it) (sqlit_slot it) (sqlit_rlevel it) stats suffs dislv disens
      where
        desc = "" -- foldMap (B.append "\n") (sp_desc <$> 
        stats = (sqlit_stats it) -- ++ getSpellStats <$> (
        suffs = maybe [] L.concat $ do
            suid <- sqlit_suffs it
            pid <- sqlit_props it
            let smap = map_suffmap maps
            return $ foldMap maybeToList $ 
                [Ma.lookup (Left suid) smap, Ma.lookup (Right pid) smap]
        (dislv, disens) = maybe (Nothing, []) (\(a,b) -> (Just a, b)) $ do
            (lv, id) <- sqlit_disen it
            encs <- M.lookup id (sqls_disench sql)
            return (lv, sqldis_drops encs)

-- sourced, serialized data

dbcSources :: DBCSources
dbcSources = DBCSources spells dbcSuffixes dbcProperties dbcEnchantments

sqlSources :: SQLSources
sqlSources = SQLSources sqlItems sqlSuffixes sqlDisenchants gameObjects

creatures :: IntMap Creature
creatures = $(serIn "creatures.gz")

gameObjects :: IntMap GameObject
gameObjects = $(serIn "gameObjects.gz")

maps :: Maps
maps = Maps suffixMap

items :: IntMap Item
items = $(serIn "items.gz")

suffixMap :: SuffixMap
suffixMap = $(serIn "suffixMap.gz")

spells :: IntMap Spell
spells = $(serIn "spells.gz")

dbcSuffixes :: IntMap DBCSuffix 
dbcSuffixes = $(serIn "dbcSuffixes.gz")

dbcProperties :: IntMap DBCProperty
dbcProperties = $(serIn "dbcProperties.gz")

dbcEnchantments :: IntMap DBCEnchantment
dbcEnchantments = $(serIn "dbcEnchantments.gz")

sqlItems :: IntMap SQLItem
sqlItems = $(serIn "sqlItems.gz")

sqlSuffixes :: IntMap SQLSuffix
sqlSuffixes = $(serIn "sqlSuffixes.gz")

sqlDisenchants :: IntMap SQLDisenchant
sqlDisenchants = $(serIn "sqlDisenchants.gz")


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

seq' x = seq x x

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

getItemSpells :: [MySQLValue] -> [Int]
getItemSpells e = do
    i <- fs <$> inds e [66,73,80,87,94]
    if i /= 0 then
        return i
    else
        []
                    
groupSuffixes :: [Suffix] -> [Suffix]
groupSuffixes xs = do
    (suf, s) <- group' $ (\s' -> (su_suffix s', s')) <$> xs
    return $ merge suf s where
        merge nam ss = Suffix nam (mergeChances ss) (maxStats ss)
        mergeChances ss = 100 - 100*(L.product $ (/100) . (100-) <$> su_chance <$> ss)
        maxStats ss = (\(a,b) -> (a, L.maximum b)) <$> group' (L.sort $ foldMap su_stats ss)

-- getSuffixMap :: Mappings -> SuffixMap
getSuffixMap m = M.fromList $ L.concat $
    [ do
        (k, ps) <- M.toList i2s
        let s = do
            (i,c) <- ps
            maybeToList $ do
                si <- M.lookup (fromIntegral i) sm
                es <- traverse (flip M.lookup em) (fromIntegral <$> dbcsu_enchs si)
                return $ getSuffix ss (dbcsu_suffix si) c (es >>= dbcen_stats) (es >>= dbcen_spells)
        return (k, L.reverse $ L.sortOn su_chance $ groupSuffixes s)
    , do
        (k, ps) <- M.toList i2p
        let s = do
            (i,c) <- ps
            maybeToList $ do
                pi <- M.lookup (fromIntegral i) pm
                es <- traverse (flip M.lookup em) (fromIntegral <$> dbcpo_enchs pi)
                return $ getSuffix ss (dbcpo_suffix pi) c (es >>= dbcen_stats) (es >>= dbcen_spells)
        return (k, L.reverse $ L.sortOn su_chance $ groupSuffixes s)
    ] where 
        ss = m_spells m
        i2s = m_i2s m
        i2p = m_i2p m
        sm = m_suffixes m
        pm = m_props m
        em = m_enchs m

--     let map = M.fromList $ flip fmap es $ \[MySQLInt32U i, MySQLInt32U e, MySQLFloat c] -> 
--           (fromIntegral i, (M.lookup (fromIntegral i) (m_suffixes m), M.lookup (fromIntegral i) (m_props m)))
--         f mmt = (\(s, s')
--     (suf,enchs) <- join $ case id of
--         Left i -> case M.lookup (fromIntegral i) map of
--             Just (Just ss, _) -> (se_suffix ss, su_enchs ss)
--             _ -> ([],[])
--         Right i -> case M.lookup (fromIntegral i) map of
--             Just (_, Just ss) -> (pe_suffix ss, pe_enchs ss)
--             _ -> ([],[])
--     in undefined

-- getItem :: Mappings -> [MySQLValue] -> Item
-- getItem m e =
--     let iid = fs (e!!0)
--         iname = (\(MySQLText t) -> encodeUtf8 t) (e!!4)
--         iqual = toEnum $ fs (e!!6)
--         islot = toEnum $ fs (e!!12)
--         weaponstats = 
--             if isWeapon islot then 
--                 [(Damage, (fs (e!!50) + fs (e!!51)) `quot` 2), (Speed, fs (e!!63))]
--             else
--                 []
--         imat = toEnum $ fs (e!!108)
--         istats = weaponstats ++ (getMainStats e) ++ (getRes e)
--         isuffs = L.concat $ maybeToList $ M.lookup iid (m_sufmap m)
--         ilevel = fs (e!!15)
--         rlevel = fs (e!!16)
--         iqs = M.filter (\q -> L.any (== iid) (qt_items q)) (m_quests m)
--         reqlevel = if M.null iqs then 
--                       rlevel 
--                     else 
--                       max rlevel (qt_level $ snd $ L.head $ M.toList iqs)
--         reqlevel' = if iqual > Uncommon && reqlevel == 0 then -1 else reqlevel
--         ispells = getItemSpells e
--         ispells' = catMaybes $ flip M.lookup (m_spells m) <$> ispells
--         istats' = catMaybes $ getSpellStats <$> ispells'
--     in Item iid iname (foldMap (B.append "\n") (sp_desc <$> ispells')) iqual 
--             ilevel imat islot (istats++istats') isuffs rlevel


-- getItems :: IO (M.IntMap Item)
-- getItems = do
    -- irs <- evaluate =<< loadResult "item_template.gz"
    -- m <- loadMappings
    -- let ret = M.fromList $ fmap (\x -> (iid x, x)) $ getItem m <$> irs
    -- deepseq ret (return ret)


just :: (Eq a, Num a) => a -> Maybe a
just n = if n/=0 then Just n else Nothing

just' :: (Eq b, Num b) => a -> b -> Maybe (a,b)
just' a b = (,) a <$> just b

inds :: [a] -> [Int] -> [a]
inds entry indices = (!!) <$> pure entry <*> indices

fs :: (Integral a, Num a) => MySQLValue -> a
fs i = case i of
    MySQLInt8 n -> fromIntegral n
    MySQLInt8U n -> fromIntegral n
    MySQLInt16 n -> fromIntegral n
    MySQLInt16U n -> fromIntegral n
    MySQLInt32 n -> fromIntegral n
    MySQLInt32U n -> fromIntegral n
    MySQLInt64 n -> fromIntegral n
    MySQLInt64U n -> fromIntegral n
    MySQLFloat n -> ceiling n

ffs :: MySQLValue -> Float
ffs (MySQLFloat i) = i

cmpLists :: (Eq a, Num b, Ord b) => [(a, b)] -> [(a,b)] -> Ordering
cmpLists a b = compare 0 $ L.sum $ (\(i,n) (i',n') -> if i == i' then n'-n else 0) <$> a <*> b

group' :: (Eq a) => [(a,b)] -> [(a,[b])]
group' xs = do
    xs' <- L.groupBy (\a b -> fst a == fst b) xs
    let k = fst $ L.head xs'
    return (k, snd <$> xs')
