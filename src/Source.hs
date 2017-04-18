{-# LANGUAGE OverloadedStrings #-}

module Source where

import Codec.Compression.GZip

import Control.DeepSeq
import Control.Exception (evaluate)

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

import qualified System.IO.Streams as S

import Types
import DBC
import SQL

data DBCSources = DBCSources
    { dbcs_spells     :: IntMap Spell
    , dbcs_suffixes   :: IntMap SuffixEntry
    , dbcs_properties :: IntMap PropertyEntry
    , dbcs_enchants   :: IntMap EnchantmentEntry
    }

data SQLSources = SQLSources
    { sqls_items      :: IntMap SQLItem
    }

q :: Query -> IO [[MySQLValue]]
q tab = do
    conn <- connect defaultConnectInfo { ciUser = "guest"
        , ciHost = "192.168.1.124", ciDatabase = "world" }
    ret <- S.toList . snd =<< query_ conn tab
    close conn
    return ret

queryI2s :: IO (M.IntMap [(SuffixId, Float)])
queryI2s = 
    queryToMap <$> q "SELECT `item_template`.`entry`, `item_enchantment_template`.`ench`, `item_enchantment_template`.`chance` FROM `item_enchantment_template`,`item_template` WHERE `item_template`.`RandomSuffix` = `item_enchantment_template`.`entry`"

queryI2p :: IO (M.IntMap [(PropertyId, Float)])
queryI2p =
    queryToMap <$> q "SELECT `item_template`.`entry`, `item_enchantment_template`.`ench`, `item_enchantment_template`.`chance` FROM `item_enchantment_template`,`item_template` WHERE `item_template`.`RandomProperty` = `item_enchantment_template`.`entry`"

queryToMap res =
    let f xs [] = [(fs$ xs!!0, [(fs$ xs!!1, ffs$ xs!!2)])]
        f xs ((j,ys):zs) = 
            let i = fs (xs!!0)
                x = fs (xs!!1)
                c = ffs (xs!!2)
            in if i == j then (j,(x,c):ys):zs else (i,[(x,c)]):(j,ys):zs
    in M.fromList $ L.foldr f [] res

queryGameObjects :: IO GameObjects
queryGameObjects = do
    q' <- q "SELECT `entry`,`name`,`position_x`,`position_y`,`position_z`,`map` FROM `gameobject`,`gameobject_template` WHERE `id` = `entry` ORDER BY `entry`"
    return (getGO <$> q') where
        getGO q =
            let i = fs (q!!0)
                n = (\(MySQLText t) -> encodeUtf8 t) (q!!1)
                [x,y,z] = ffs <$> [q!!2, q!!3, q!!4]
                m = fs (q!!5)
            in GameObject i n (Point x y z m)
                

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

enc :: Serialize a => a -> ByteString
enc = BL.toStrict . compress . encodeLazy 

save :: Serialize a => FilePath -> a -> IO ()
save file res = B.writeFile file $ enc res

dec :: Serialize a => BL.ByteString -> a
dec = either error id . decode . BL.toStrict . decompress 

dec' :: Serialize a => String -> a
dec' str = seq ret ret where ret = dec $ BLC.pack str

load :: Serialize a => FilePath -> IO a
load file = dec <$> BL.readFile file

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

saveSuffixes :: M.IntMap SuffixEntry -> IO ()
saveSuffixes = save "suffixes.gz"

loadSuffixes :: IO (M.IntMap SuffixEntry)
loadSuffixes = load "suffixes.gz"

saveProperties :: M.IntMap PropertyEntry -> IO ()
saveProperties = save "properties.gz"

loadProperties :: IO (M.IntMap PropertyEntry)
loadProperties = load "properties.gz"

saveEnchantments :: M.IntMap EnchantmentEntry -> IO ()
saveEnchantments = save "enchantments.gz"

loadEnchantments :: IO (M.IntMap EnchantmentEntry)
loadEnchantments = load "enchantments.gz"

saveSuffixMap :: SuffixMap -> IO ()
saveSuffixMap = save "suffixMap.gz"

loadSuffixMap :: IO SuffixMap
loadSuffixMap = load "suffixMap.gz"

saveGameObjects :: GameObjects -> IO ()
saveGameObjects = save "gameobjects.gz"

loadGameObjects :: IO GameObjects
loadGameObjects = load "gameobjects.gz"

loadItems :: IO (M.IntMap Item)
loadItems = load "items.gz"

saveItems :: M.IntMap Item -> IO ()
saveItems is = save "items.gz" is

loadMappings :: IO Mappings
loadMappings = do
    ss <- evaluate =<< loadSpells
    qs <- evaluate =<< loadQuests
    i2s <- evaluate =<< load "i2s.gz"
    i2p <- evaluate =<< load "i2p.gz"
    suff <- evaluate =<< loadSuffixes
    ps <- evaluate =<< loadProperties
    es <- evaluate =<< loadEnchantments
    sm <- evaluate =<< loadSuffixMap
    evaluate $ Mappings ss qs i2s i2p suff ps es sm

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
                    
getSuffix :: M.IntMap Spell -> ByteString -> Float -> [(Stat, Int)] -> [SpellId] -> Suffix
getSuffix ss su ch st sl = Suffix su ch $ st ++ do
    sid <- sl
    maybeToList $ do
        sp <- M.lookup (fromIntegral sid) ss
        getSpellStats sp

groupSuffixes :: [Suffix] -> [Suffix]
groupSuffixes xs = do
    (suf, s) <- group' $ (\s' -> (su_suffix s', s')) <$> xs
    return $ merge suf s where
        merge nam ss = Suffix nam (mergeChances ss) (maxStats ss)
        mergeChances ss = 100 - 100*(L.product $ (/100) . (100-) <$> su_chance <$> ss)
        maxStats ss = (\(a,b) -> (a, L.maximum b)) <$> group' (L.sort $ foldMap su_stats ss)

getSuffixMap :: Mappings -> SuffixMap
getSuffixMap m = M.fromList $ L.concat $
    [ do
        (k, ps) <- M.toList i2s
        let s = do
            (i,c) <- ps
            maybeToList $ do
                si <- M.lookup (fromIntegral i) sm
                es <- traverse (flip M.lookup em) (fromIntegral <$> se_enchs si)
                return $ getSuffix ss (se_suffix si) c (es >>= ee_stats) (es >>= ee_spells)
        return (k, L.reverse $ L.sortOn su_chance $ groupSuffixes s)
    , do
        (k, ps) <- M.toList i2p
        let s = do
            (i,c) <- ps
            maybeToList $ do
                pi <- M.lookup (fromIntegral i) pm
                es <- traverse (flip M.lookup em) (fromIntegral <$> pe_enchs pi)
                return $ getSuffix ss (pe_suffix pi) c (es >>= ee_stats) (es >>= ee_spells)
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
