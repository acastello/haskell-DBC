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
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char
import qualified Data.IntMap as M
import Data.IntMap (IntMap)
import qualified Data.List as L
import qualified Data.Map as Ma
import Data.Maybe
import Data.Scientific
import Data.Serialize
import Data.String
import Data.Text (Text (..))
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
    { dbcs_spells     :: IntMap DBCSpell
    , dbcs_casttimes  :: IntMap DBCCastTime
    , dbcs_dpinfo     :: IntMap DBCDisplayInfo
    , dbcs_scastats   :: IntMap DBCScalingStat
    , dbcs_suffixes   :: IntMap DBCSuffix
    , dbcs_properties :: IntMap DBCProperty
    , dbcs_enchants   :: IntMap DBCEnchantment
    } deriving Generic
instance Serialize DBCSources

instance Make DBCSources where
    file = undefined
    make = $(liftN "make" 7) DBCSources 
    load' = $(liftN "load'" 7) DBCSources
    save' (DBCSources a b c d e f g) = 
        save' a >> save' b >> save' c >> save' d >> save' e >> save' f >> save' g

data SQLSources = SQLSources
    { sqls_items      :: IntMap SQLItem
    , sqls_suffmap    :: IntMap SQLSuffix
    , sqls_disench    :: IntMap SQLDisenchant
    , sqls_gobjs      :: IntMap GameObject
    , sqls_creats     :: IntMap Creature
    } deriving Generic
instance Serialize SQLSources

instance Make SQLSources where
    file _ = undefined
    make = $(liftN "make" 5) SQLSources
    load' = $(liftN "load'" 5) SQLSources 
    save' (SQLSources a b c d e) = save' a >> save' b >> save' c >> save' d >> save' e

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
    , final_spells    :: IntMap Spell
    } deriving Generic
instance Serialize FinalData

instance Make FinalData where
    make = $(liftN "make" 2) FinalData
    load' = $(liftN "load'" 2) FinalData
    save' (FinalData a b) = save' a >> save' b

instance Make (IntMap Spell) where
    file _ = "spells.gz"
    make = return $ fmap (makeSpell dbcSources sqlSources maps) (dbcs_spells dbcSources)

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
    (sqlit_mat it) (sqlit_slot it) (sqlit_rlevel it) (sqlit_price it) stats 
    spells suffs dislv disens icon
      where
        desc = "" -- foldMap (B.append "\n") (dbcsp_desc <$> 
        stats = (maybe [] dbcss_stats 
                    (M.lookup (sqlit_scaling it) (dbcs_scastats dbc)))
                ++ (sqlit_stats it) ++ catMaybes ( 
                (\s -> M.lookup s (dbcs_spells dbc) >>= getSpellStats) <$> (sqlit_spells it))
        spells = sqlit_spells it
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
        icon = maybe "INV_Misc_QuestionMark" (BC.map toLower . dbcdi_icon) $ 
              M.lookup (sqlit_display it) (dbcs_dpinfo dbc)

makeSpell :: DBCSources -> SQLSources -> Maps -> DBCSpell -> Spell
makeSpell dbc sql maps sp = Spell
    (dbcsp_id sp) (dbcsp_val sp) (dbcsp_scho sp) (dbcsp_scho2 sp) 
    (dbcsp_reag sp) (dbcsp_prod sp) (dbcsp_name sp) (dbcsp_desc sp) 
    (maybe 0 dbcct_time $ M.lookup (dbcsp_castid sp) (dbcs_casttimes dbc))

-- sourced, serialized data

dbcSources :: DBCSources
dbcSources = DBCSources dbcSpells dbcCastTimes dbcDisplayInfo dbcScalingStats dbcSuffixes dbcProperties dbcEnchantments

sqlSources :: SQLSources
sqlSources = SQLSources sqlItems sqlSuffixes sqlDisenchants gameObjects creatures

maps :: Maps
maps = Maps suffixMap

finals :: FinalData
finals = FinalData items spells

creatures :: IntMap Creature
creatures = $(serIn "creatures.gz")

gameObjects :: IntMap GameObject
gameObjects = $(serIn "gameObjects.gz")

items :: IntMap Item
items = $(serIn "items.gz")

spells :: IntMap Spell
spells = $(serIn "spells.gz")

suffixMap :: SuffixMap
suffixMap = $(serIn "suffixMap.gz")

dbcSpells :: IntMap DBCSpell
dbcSpells = $(serIn "dbcSpells.gz")

dbcCastTimes :: IntMap DBCCastTime
dbcCastTimes = $(serIn "dbcCastTimes.gz")

dbcDisplayInfo :: IntMap DBCDisplayInfo
dbcDisplayInfo = $(serIn "dbcDisplayInfo.gz")

dbcScalingStats :: IntMap DBCScalingStat
dbcScalingStats = $(serIn "dbcScalingStats.gz")

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

liftM7 fun a b c d e f g = do
    a' <- a
    b' <- b
    c' <- c
    d' <- d
    e' <- e
    f' <- f
    g' <- g
    return $ fun a' b' c' d' e' f' g'
