{-# LANGUAGE  OverloadedStrings
            , StandaloneDeriving
            , DeriveGeneric
            , FlexibleInstances
            , FlexibleContexts
            , DefaultSignatures
            , TypeOperators #-}

module Core
    ( module Core
    , module DBC
    , module SQL 
    , IntMap
    , first
    , second
    , bimap
    ) where 

import Codec.Compression.GZip

import Control.DeepSeq
import Control.Monad 

import Data.ByteString.Char8 hiding (foldl1, index)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Function
import Data.Bifunctor
import Data.Int
import qualified Data.Map as Ma
import qualified Data.IntMap as M
import Data.IntMap (IntMap(..))
import qualified Data.List as L
import qualified Data.Map as Ma
import Data.Maybe
import qualified Data.Vector as V
import Data.Serialize
import Data.Word

import GHC.Generics

import Text.Printf (printf)

import DBC
import SQL

class Serialize a => Make a where
    make  :: IO a
    file  :: a -> FilePath
    file = undefined
    save' :: a -> IO ()
    save' e = save (file e) e
    load' :: IO a
    load' = load'' undefined where
        load'' :: Make a => a -> IO a
        load'' x = load (file x)

data Slot = NoSlot | Head | Neck | Shoulder | Back | Chest | Waist | Legs 
          | Wrists | Hands | Feet | Finger | Trinket | MainHand | OffHand 
          | Weapon | TwoHand | Ranged | Bag | HandHeld | Thrown | Ammo | Relic 
          | Quiver | Shirt | Tabard | Shield | UnknownSlot Int
    deriving (Show, Read, Eq, Generic, Ord)
instance Serialize Slot
instance NFData Slot

isWeapon :: Slot -> Bool
isWeapon slot = L.any (== slot) [MainHand, OffHand, Weapon, TwoHand, Ranged]

data Material = NotArmor | Cloth | Leather | Mail | Plate
    deriving (Eq, Ord, Generic)
instance Serialize Material
instance NFData Material

instance Show Material where
    show NotArmor = ""
    show Cloth = "Cloth"
    show Leather = "Leather"
    show Mail = "Mail"
    show Plate = "Plate"

instance Enum Material where
    fromEnum = undefined
    toEnum n = case n of
        5 -> Mail
        6 -> Plate
        7 -> Cloth
        8 -> Leather
        _ -> NotArmor

appendAT :: String -> Material -> String
appendAT pre at =
    let s = show at
    in if L.null s then
        pre
    else
        pre ++ ", " ++ s

data Stat = Mana | HP | Agility | Strength | Intellect | Spirit | Stamina 
    | Armor | Defense | Dodge | Parry | Block | Hit | Crit | Resilence | Haste 
    | Expertise | AttackPower | HealingPower | ManaPer5 | SpellPower | ArmorPen
    | Vitality | SpellPen | BlockValue | Speed | Damage
    | HolyRes | ShadowRes | FireRes | FrostRes | NatureRes | ArcaneRes 
    | UnknownStat Int
    deriving (Eq, Read, Generic, Ord, Show)
instance Serialize Stat
instance NFData Stat

data Quality = Poor | Common | Uncommon | Rare | Epic | Legendary
    | Artifact | Heirloom
    deriving (Eq, Ord, Show, Read, Generic)
instance Serialize Quality
instance NFData Quality 

instance Enum Quality where
    fromEnum = undefined
    toEnum i = case i of
        0 -> Poor
        1 -> Common
        2 -> Uncommon
        3 -> Rare
        4 -> Epic
        5 -> Legendary
        6 -> Artifact
        7 -> Heirloom

qualc :: Item -> String
qualc i = col $ case it_qual i of
    Poor      -> []
    Common    -> [0,1]
    Uncommon  -> [1,32]
    Rare      -> [1,34]
    Epic      -> [1,35]
    Legendary -> [1,33]
    _         -> [1,36]

data Class = Mage | Priest | Warlock | Druid | Rogue | Hunter | Shaman
    | DeathKnight | Paladin | Warrior
    deriving (Show, Read, Generic)
instance Serialize Class
instance NFData Class

data Faction = Alliance | Horde
    deriving (Read, Generic)

instance Show Faction where
    show Alliance = col [1,37,44] ++ "[Alliance]" ++ col []
    show Horde = col [1,30,41] ++ "[Horde]" ++ col []
instance Serialize Faction
instance NFData Faction

data Bonding = BoP | BoE | BtA 

type ItemId = Int
type Level = Int
type ItemLevel = Level

data SQLItem = SQLItem
    { sqlit_id      :: ItemId
    , sqlit_name    :: ByteString
    , sqlit_level   :: ItemLevel
    , sqlit_qual    :: Quality
    , sqlit_mat     :: Material
    , sqlit_slot    :: Slot
    , sqlit_rlevel  :: Level
    , sqlit_price   :: Int
    , sqlit_stats   :: [(Stat, Int)]
    , sqlit_scaling :: Int
    , sqlit_spells  :: [SpellId]
    , sqlit_suffs   :: Maybe SuffixId
    , sqlit_props   :: Maybe PropertyId
    , sqlit_disen   :: Maybe (Level, SQLDisenchantId)
    , sqlit_display :: Int
    } deriving (Generic, Show)
instance Serialize SQLItem
instance NFData SQLItem

instance SQL SQLItem where
    queryText = \_ -> simpleSelect "item_template"
      [ "entry", "name", "ItemLevel", "Quality", "Material", "InventoryType"
      , "RequiredLevel", "SellPrice"
      -- stat count, up to 10
      , "StatsCount"
      -- stat types
      , "stat_type1", "stat_type2", "stat_type3", "stat_type4", "stat_type5"
      , "stat_type6", "stat_type7", "stat_type8", "stat_type9", "stat_type10"
      -- stat values
      , "stat_value1", "stat_value2", "stat_value3" , "stat_value4"
      , "stat_value5", "stat_value6", "stat_value7" , "stat_value8"
      , "stat_value9", "stat_value10" 
      
      , "RandomSuffix", "RandomProperty", "RequiredDisenchantSkill"
      , "DisenchantId" 
      , "spellid_1", "spellid_2", "spellid_3", "spellid_4", "spellid_5"
      , "scalingStatDistribution", "displayid" ]
    fromResult = do
        id      <- sql_fi 0
        name    <- sql_bs 1
        level   <- sql_fi 2
        qual    <- toEnum <$> sql_fi 3
        mat     <- toEnum <$> sql_fi 4
        slot    <- toEnum <$> sql_fi 5
        rlevel  <- sql_fi 6
        price   <- sql_fi 7
        n       <- sql_fi 8
        stnames <- mapM (fmap toEnum . sql_fi) [9..18]
        stvals  <- mapM sql_fi [19..28]
        let stats = L.take n $ L.zip stnames stvals
        suffs   <- maybe0 <$> sql_fi 29
        props   <- maybe0 <$> sql_fi 30
        disenlv <- sql_fi 31
        disenid <- sql_fi 32
        let disen = 
              if disenlv /= -1 && disenid > 0 then 
                  Just (disenlv, disenid) 
              else 
                  Nothing
        spells <- L.filter (/= 0) <$> mapM sql_fi [33,34,35,36,37]
        scaling <- sql_fi 38
        dpy <- sql_fi 39
        return $ SQLItem id name level qual mat slot rlevel price stats scaling
                          spells suffs props disen dpy
    finalResult = index' sqlit_id

instance Make (IntMap SQLItem) where
    file _ = "sqlItems.gz"
    make = makeSQL

data Item = Item 
    { it_id     :: ItemId
    , it_name   :: ByteString 
    , it_desc   :: ByteString
    , it_level  :: ItemLevel 
    , it_qual   :: Quality
    , it_mat    :: Material
    , it_slot   :: Slot
    , it_rlevel :: Level
    , it_price  :: Int
    , it_stats  :: [(Stat, Int)] 
    , it_spells :: [SpellId]
    , it_suffs  :: [Suffix]
    , it_disenlv:: Maybe Int
    , it_disen  :: [(ItemId, Double)]
    , it_icon   :: ByteString
    } deriving Generic
instance Serialize Item
instance NFData Item

instance Show Item where
    show i = printf "%s#%s%-5d %s (%s) %s%s%s%s%s\n" 
        (col [1,36]) (col [0,36]) (it_id i) 
        (qualc i ++ (unpack $ it_name i) ++ col [])
        (appendAT (show $ it_slot i) (it_mat i)) 
        (showStats $ it_stats i) (col [32]) (tab' $ it_desc i) (col [])
        (foldMap ("\n        " ++) (show <$> it_suffs i))
        
-- makeItem dbc sql it = Item 
    -- (sqlit_id it) (sqlit_name it) desc (sqlit_level it)
    -- lv qual mat
    -- slot rlevel
    -- stats

data DBCDisplayInfo = DBCDisplayInfo
    { dbcdi_id    :: Int
    , dbcdi_icon  :: ByteString
    } deriving (Show, Generic)
instance Serialize DBCDisplayInfo

instance DBCItem DBCDisplayInfo where
    cast = liftM2 DBCDisplayInfo (castAt 0) (castAt 5)

instance DBCFile DBCDisplayInfo where
    dbcIndex = dbcdi_id
    dbcFile _ = "ItemDisplayInfo.dbc"

instance Make (IntMap DBCDisplayInfo) where
    make = makeDBC
    file _ = "dbcDisplayInfo.gz"

data DBCSpellIcon = DBCSpellIcon
    { dbcsi_id      :: Int
    , dbcsi_icon    :: ByteString
    } deriving (Show, Generic)
instance Serialize DBCSpellIcon

instance DBCItem DBCSpellIcon where
    cast = liftM2 DBCSpellIcon (castAt 0) (castAt 1)

instance DBCFile DBCSpellIcon where
    dbcIndex = dbcsi_id
    dbcFile _ = "SpellIcon.dbc"

instance Make (IntMap DBCSpellIcon) where
    make = makeDBC
    file _ = "dbcSpellIcons.gz"

data DBCCastTime = DBCCastTime
    { dbcct_id      :: Int
    , dbcct_time    :: Double
    } deriving (Show, Generic)
instance Serialize DBCCastTime

instance DBCItem DBCCastTime where
    cast = liftM2 DBCCastTime (castAt 0) ((/1000) . fi <$> castW32At 1)

instance DBCFile DBCCastTime where
    dbcIndex = dbcct_id
    dbcFile _ = "SpellCastTimes.dbc"

instance Make (IntMap DBCCastTime) where
    file _ = "dbcCastTimes.gz"
    make = makeDBC

type SpellId = Int

data DBCSpell = DBCSpell
    { dbcsp_id      :: SpellId
    , dbcsp_val     :: Int32
    , dbcsp_scho    :: Word32
    , dbcsp_scho2   :: Word32
    , dbcsp_reag    :: [(ItemId, Int)]
    , dbcsp_prod    :: [(ItemId, Double)]
    , dbcsp_name    :: ByteString
    , dbcsp_desc    :: ByteString
    , dbcsp_castid  :: Int
    , dbcsp_iconid  :: Int
    } deriving (Show, Generic)
instance Serialize DBCSpell where
instance NFData DBCSpell where

instance DBCFile DBCSpell where
    dbcIndex = dbcsp_id
    dbcFile _ = "Spell.dbc"

instance DBCItem DBCSpell where
    cast = do
        id <- (fi :: Word32 -> Int) <$> castAt 0
        castid <- castAt 28
        n <- (+1) <$> castAt 80
        t1 <- castAt 95
        t2 <- castAt 110
        icon <- castAt 133
        name <- castAt 136
        desc <- castAt 170
        reag <- do
            ids <- mapM castAt [52..59]
            quants <- mapM (fmap (fi :: Word32 -> Int) . castAt) [60..67] 
            return $ L.filter (\(a,b) -> a /= 0 && b /= 0) $ L.zip ids quants
        prod <- do
            ids <- L.filter (/= 0) <$> mapM castAt [107..109]
            quants <- mapM (fmap (fi :: Word32 -> Double) . castAt) [80..82]
            dice <- mapM 
              (fmap (\x -> (1 + fi x)/2) . (castAt :: Int -> DBCGet Word32)) 
              [74..76]
            return $ L.zip ids (L.zipWith (+) dice quants)
        return $ DBCSpell id n t1 t2 reag prod name
            (replaceSubstring "$s1" (pack $ show n) desc) castid icon

instance Make (IntMap DBCSpell) where
    file _ = "dbcSpells.gz"
    make = makeDBC

data Spell = Spell
    { sp_id     :: SpellId
    , sp_val    :: Int32
    , sp_scho   :: Word32
    , sp_scho2  :: Word32
    , sp_reag   :: [(ItemId, Int)]
    , sp_prod   :: [(ItemId, Double)]
    , sp_name   :: ByteString
    , sp_desc   :: ByteString
    , sp_cast   :: Double
    , sp_icon   :: ByteString
    } deriving (Show, Generic)
instance Serialize Spell

-- instance Gettable DBCSpell where
--     get' strs = do
--         id <- get' strs
--         skip (4*79)
--         n <- get' strs
--         skip (4*14)
--         t1 <- get' strs
--         skip (4*14) 
--         t2 <- get' strs
--         skip (4*59)
--         desc <- get' strs
--         return $ DBCSpell id (n+1) (t1,t2) (replaceSubstring "$s1" (pack $ show (n+1)) desc)

data Quest = Quest 
    { qt_id     :: Int
    , qt_name   :: ByteString
    , qt_level  :: Level
    , qt_fac    :: Maybe Faction
    , qt_items  :: [Int]
    } deriving (Show, Generic)
instance Serialize Quest 
instance NFData Quest 

data DBCScalingStat = DBCScalingStat
    { dbcss_id      :: Int
    , dbcss_stats   :: [(Stat, Int)]
    } deriving (Show, Generic)
instance Serialize DBCScalingStat
instance NFData DBCScalingStat

instance DBCFile DBCScalingStat where
    dbcIndex = dbcss_id
    dbcFile _ = "ScalingStatDistribution.dbc"

instance DBCItem DBCScalingStat where
    cast = do
        id <- fi <$> castWord32
        stats <- mapM castAt [1..10]
        vals <- mapM castAt [11..20]
        let f x y = if x == -1 || y /= 0 then 
                    Just (toEnum x, y*130`quot`10000) else Nothing
            pairs = catMaybes $ L.zipWith f stats vals
        return $ DBCScalingStat id pairs

instance Make (IntMap DBCScalingStat) where
    file _ = "dbcScalingStats.gz"
    make = makeDBC

type SuffixId = Int
data DBCSuffix = DBCSuffix
    { dbcsu_id      :: SuffixId
    , dbcsu_suffix  :: ByteString
    , dbcsu_enchs   :: [EnchantmentId]
    } deriving (Show, Generic)
instance Serialize DBCSuffix 
instance NFData DBCSuffix 

instance DBCFile DBCSuffix where
    dbcIndex = dbcsu_id
    dbcFile _ = "ItemRandomSuffix.dbc"

instance DBCItem DBCSuffix where
    cast = do
        id <- fi <$> castWord32
        suffix <- castAt 1
        es <- mapM (fmap fi . castW32At) [19..23]
        return $ DBCSuffix id suffix (L.filter (/=0) es)

instance Make (IntMap DBCSuffix) where
    file _ = "dbcSuffixes.gz"
    make = makeDBC

-- instance Gettable DBCSuffix where
--     get' strs = do
--         id <- get' strs
--         suffix <- get' strs
--         skip (4*17)
--         es <- replicateM 5 (get' strs)
--         return $ DBCSuffix id suffix (L.filter (/=0) es)

type PropertyId = Int
data DBCProperty = DBCProperty 
    { dbcpo_id     :: PropertyId
    , dbcpo_suffix :: ByteString
    , dbcpo_enchs  :: [EnchantmentId]
    } deriving (Show, Generic)
instance Serialize DBCProperty 
instance NFData DBCProperty 

instance DBCFile DBCProperty where
    dbcIndex = dbcpo_id
    dbcFile _ = "ItemRandomProperties.dbc"

instance DBCItem DBCProperty where
    cast = do
        id <- fi <$> castWord32
        suffix <- castAt 1
        es <- mapM (fmap fi . castW32At) [2..4]
        return $ DBCProperty id suffix (L.filter (/=0) es)

instance Make (IntMap DBCProperty) where
    file _ = "dbcProperties.gz"
    make = makeDBC

type EnchantmentId = Int
data DBCEnchantment = DBCEnchantment
    { dbcen_id     :: EnchantmentId
    , dbcen_stats  :: [(Stat,Int)]
    , dbcen_spells :: [SpellId]
    } deriving (Show, Generic)
instance Serialize DBCEnchantment 
instance NFData DBCEnchantment 

instance DBCFile DBCEnchantment where
    dbcIndex = dbcen_id
    dbcFile _ = "SpellItemEnchantment.dbc"

instance DBCItem DBCEnchantment where
    cast = do
        id <- cast
        ts <- mapM castW32At [2..4]
        ns <- mapM castAt [5..7]
        ss <- mapM castW32At [11..13]
        let (stats, spells) = (\(a,b) -> (L.concat a, L.concat b)) $ L.unzip $
                zipWith3 (\t n s -> case t of
                    5 -> ([(toEnum $ fromIntegral s, n)], [])
                    4 -> case s of
                        1 -> ([(HolyRes, n)], [])
                        2 -> ([(FireRes, n)], [])
                        3 -> ([(NatureRes, n)], [])
                        4 -> ([(FrostRes, n)], [])
                        5 -> ([(ShadowRes, n)], [])
                        6 -> ([(ArcaneRes, n)], [])
                        _ -> ([], [])
                    3 -> ([], [fromIntegral s])
                    _ -> ([], [])) ts ns ss
        return $ DBCEnchantment id stats spells

instance Make (IntMap DBCEnchantment) where
    file _ = "dbcEnchantments.gz"
    make = makeDBC

data SQLSuffix = SQLSuffix
    { sqlsu_id    :: SuffixId
    , sqlsu_suffs :: [(SuffixId, Float)]
    } deriving (Generic)
instance Serialize SQLSuffix

instance SQL SQLSuffix where
    queryText _ = simpleSelect "item_enchantment_template" ["entry", "ench", "chance"]
    fromResult = do
        id <- sql_fi 0
        suffid <- sql_fi 1
        chance <- sql_float 2 
        return $ SQLSuffix id [(suffid, chance)]
    finalResult xs = M.fromListWith 
          (\a b -> a { sqlsu_suffs = sqlsu_suffs a ++ sqlsu_suffs b })
          $ (\e -> (sqlsu_id e, e)) <$> xs

instance Make (IntMap SQLSuffix) where
    file _ = "sqlSuffixes.gz"
    make = makeSQL

data Suffix = Suffix
    { su_suffix :: ByteString
    , su_chance :: Float
    , su_stats  :: [(Stat, Int)]
    } deriving Generic
instance Serialize Suffix 
instance NFData Suffix

instance Show Suffix where
    show su = printf "%s%s%s (%2.1f %%) %s%s"
          (col [1,3,32]) (unpack (su_suffix su)) (col [0,3]) 
          (su_chance su) (showStats (su_stats su)) (col [23])

type SuffixMap = Ma.Map (Either SuffixId PropertyId) [Suffix]

getSuffix :: M.IntMap DBCSpell -> ByteString -> Float -> [(Stat, Int)] -> [SpellId] -> Suffix
getSuffix ss su ch st sl = Suffix su ch $ st ++ do
    sid <- sl
    maybeToList $ do
        sp <- M.lookup (fromIntegral sid) ss
        getSpellStats sp

data Point = Point
    { p_x :: Float
    , p_y :: Float
    , p_z :: Float
    , p_m :: Int
    } deriving (Eq, Generic)
instance Serialize Point
instance NFData Point

instance Show Point where
    show p = printf "%s(%s%.3f%s,%s %.3f%s,%s %.3f%s)%s %s%-3d%s %s%s%s" 
        (col [1]) (col []) (p_x p) (col [1]) (col []) (p_y p) (col [1]) (col []) 
        (p_z p) (col [1]) (col [])  (col [32]) (p_m p) (col [38]) (col [1])
        (maybe "<unknown>" id $ M.lookup (p_m p) mapMap) (col [])

p2t :: Point -> (Float, Float, Float, Word32)
p2t Point { p_x = x, p_y = y, p_z = z, p_m = m } = (x,y,z,fromIntegral m)

t2p :: (Float, Float, Float, Word32) -> Point
t2p (x,y,z,m) = Point x y z (fromIntegral m)

data GameObject = GameObject
    { go_id     :: Int
    , go_guid   :: Int
    , go_name   :: ByteString
    , go_point  :: Point
    , go_cd     :: Int
    } deriving Generic
instance Serialize GameObject
instance NFData GameObject 

instance Eq GameObject where
    (==) = (==) `on` go_id 

instance Ord GameObject where
    compare = compare `on` go_id

instance Show GameObject where
    show go = printf "%s#%s%d%s %s %s" (col [1,36]) (col [0,36]) 
              (go_id go) (col []) (unpack $ go_name go) (show $ go_point go)

instance SQL GameObject where
    queryText _ = "SELECT `entry`,`guid`,`name`,`position_x`,`position_y`,`position_z`,`map`,`spawntimesecs` FROM `gameobject`,`gameobject_template` WHERE `id` = `entry` ORDER BY `entry`"
    fromResult = do
        id <- sql_fi 0
        guid <- sql_fi 1
        name <- sql_bs 2
        x <- sql_float 3
        y <- sql_float 4
        z <- sql_float 5
        m <- sql_fi 6
        t <- sql_fi 7
        return $ GameObject id guid name (Point x y z m) t
    finalResult = index' go_guid

instance Make (IntMap GameObject) where
    file _ = "gameObjects.gz"
    make = makeSQL

data Creature = Creature
    { cr_id     :: Int
    , cr_guid   :: Int
    , cr_name   :: ByteString
    , cr_title  :: ByteString
    , cr_point  :: Point
    } deriving (Show, Generic)
instance Serialize Creature

instance Eq Creature where
    (==) = (==) `on` cr_id

instance SQL Creature where
    queryText _ = "SELECT `id`, `guid`, `name`, `subname`, `position_x`, `position_y`, `position_z`, `map` FROM `creature`, `creature_template` WHERE `entry` = `id`"
    fromResult = do
        id <- sql_fi 0
        guid <- sql_fi 1
        name <- sql_bs 2
        title <- sql_bs 3
        x <- sql_float 4
        y <- sql_float 5
        z <- sql_float 6
        m <- sql_fi 7
        return $ Creature id guid name title (Point x y z m)
    finalResult = index' cr_guid

instance Make (IntMap Creature) where
    file _ = "creatures.gz"
    make = makeSQL

type SQLDisenchantId = Int
data SQLDisenchant = SQLDisenchant
    { sqldis_id     :: SQLDisenchantId
    , sqldis_drops  :: [(ItemId, Double)]
    } deriving (Show, Generic)
instance Serialize SQLDisenchant

instance SQL SQLDisenchant where
    queryText _ = 
        "SELECT t.entry, item, chance, sum, mincount, maxcount  \
        \    FROM (SELECT entry,100-SUM(Chance) as SUM           \
        \        FROM `disenchant_loot_template`                 \
        \            WHERE 1 GROUP BY Entry) as s                \
        \    , `disenchant_loot_template` as t                   \
        \    WHERE t.entry = s.entry"                            
    fromResult = do
        id      <- sql_fi 0
        item    <- sql_fi 1
        chance' <- sql_double 2
        chance''<- sql_double 3
        let chance = (if chance' == 0 then chance'' else chance') / 100
        min     <- sql_fi 4
        max     <- sql_fi 5
        let amount = (min + max) / 2
        return $ SQLDisenchant id [(item, chance*amount)]
    finalResult xs = M.fromListWith
        (\a b -> a { sqldis_drops = sqldis_drops a ++ sqldis_drops b })
        $ (\e -> (sqldis_id e, e)) <$> xs

instance Make (IntMap SQLDisenchant) where
    file _ = "sqlDisenchants.gz"
    make = makeSQL

getSpellStats :: DBCSpell -> Maybe (Stat,Int)
getSpellStats sp = (\i -> (i, fromIntegral $ dbcsp_val sp)) <$> 
  case (dbcsp_scho sp, dbcsp_scho2 sp) of
    (135,126) -> Just SpellPower
    (13, n)   
      | L.any (==n) [2,4,8,16,32,64,126] -> Just SpellPower
    (99,0)    -> Just AttackPower
    (85,0)    -> Just ManaPer5
    (123,124) -> Just SpellPen
    (158,0)   -> Just BlockValue
    (161,0)   -> Just Vitality
    (189,2)   -> Just Defense
    (189,4)   -> Just Parry
    (189,8)   -> Just Dodge
    (189,224) -> Just Hit
    (189,1782)   -> Just Crit
    (189,1792)   -> Just Crit
    (189,16777216)  -> Just ArmorPen
    _ -> Nothing

-- instance Show Stat where
--     show a = (\s -> col [1] ++ s ++ col [21]) $ case a of
--         Mana      -> "mana"
--         HP        -> "hp"
--         Agility   -> "agi"
--         Strength  -> "str"
--         Intellect -> "int"
--         Spirit    -> "spi"
--         Stamina   -> "sta"
--         Defense   -> "def"
--         Dodge     -> "dge"
--         Parry     -> "par"
--         Block     -> "blk"
--         Crit      -> "crit"
--         Haste     -> "hast"
--         Hit       -> "hit"
--         Expertise -> "exp"
--         ManaPer5  -> "mp5"
--         Resilence -> "resil"
--         AttackPower   -> "attk"
--         HealingPower  -> "heal"
--         SpellPower    -> "sp"
--         BlockValue    -> "blkv"
--         Speed     -> "delay"
--         Damage    -> "dmg"
--         HolyRes   -> "holy-r"
--         ShadowRes -> "shad-r"
--         FireRes   -> "fire-r"
--         FrostRes  -> "fros-r"
--         NatureRes -> "nat-r"
--         ArcaneRes -> "arc-r"
--         ArmorPen  -> "arpen"
--         SpellPen  -> "spen"
--         Vitality  -> "hp5"
--         UnknownStat n -> col [7] ++ "?? " ++ show n ++ col [27]

instance Enum Stat where
    fromEnum = undefined
    toEnum i = case i of
        0   -> Mana
        1   -> HP
        3   -> Agility
        4   -> Strength
        5   -> Intellect
        6   -> Spirit
        7   -> Stamina
        12  -> Defense
        13  -> Dodge
        14  -> Parry
        15  -> Block
        16  -> Hit
        17  -> Hit
        18  -> Hit
        19  -> Crit
        20  -> Crit
        21  -> Crit
        28  -> Haste
        29  -> Haste
        30  -> Haste
        31  -> Hit
        32  -> Crit
        35  -> Resilence
        36  -> Haste
        37  -> Expertise
        38  -> AttackPower
        39  -> AttackPower
        41  -> HealingPower
        43  -> ManaPer5
        44  -> ArmorPen
        45  -> SpellPower
        46  -> Vitality
        47  -> SpellPen
        48  -> BlockValue
        i -> UnknownStat i

instance Enum Slot where
    fromEnum = undefined
    toEnum i = case i of
        0   -> NoSlot
        1   -> Head
        2   -> Neck
        3   -> Shoulder
        4   -> Shirt
        5   -> Chest
        6   -> Waist
        7   -> Legs
        8   -> Feet
        9   -> Wrists
        10  -> Hands
        11  -> Finger
        12  -> Trinket
        13  -> Weapon
        14  -> Shield
        15  -> Ranged
        16  -> Back
        17  -> TwoHand
        18  -> Bag
        19  -> Tabard
        20  -> Chest
        21  -> MainHand
        22  -> OffHand
        23  -> HandHeld
        24  -> Ammo
        25  -> Thrown
        26  -> Ranged
        27  -> Quiver
        28  -> Relic
        i   -> UnknownSlot i

data Spaced a b = Spaced a b

instance (Show a) => Show (Spaced a String) where
    show _ = ""

instance (Show a, Show b) => Show (Spaced a b) where
    show (Spaced a b) = (show a) ++ " " ++ (show b)

newtype SpacedL a = SpacedL [a]

instance Show a => Show (SpacedL a) where
    show (SpacedL []) = "[]"
    show (SpacedL (x:xs)) = "[" ++ show x ++ foldMap (", "++) (show <$> xs) ++ "]"

breakBy :: Char -> ByteString -> [ByteString]
breakBy c str =
    case B.break (== c) str of
        (l, "") -> [l]
        (l, r) -> l : breakBy c (B.tail r)

replaceSubstring :: ByteString -> ByteString -> ByteString -> ByteString
replaceSubstring needle rep hay =
    case breakSubstring needle hay of
        (l,"") -> l
        (l,r) -> l `B.append` rep `B.append` (replaceSubstring needle rep $ B.drop (B.length needle) r) 

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

--
-- utils
--

fr :: (Real a, Fractional b) => a -> b
fr = fromRational . toRational

col [] = col [0]
col xs = "\ESC[" ++ foldl1 (\s s' -> s ++ ";" ++ s') (show <$> xs) ++ "m\STX"

sample' = Prelude.putStrLn $ L.unlines $ fmap L.concat $ 
          (fmap (\i -> printf "%s Nigger %3d %s " (col [i]) i (col []))) 
          <$> formatting numbers where
    numbers = L.concat [[0..5], [7..9], [30..38], [40..48]
                       , [90..98], [100..107 :: Int]]
    formatting str = (\(b,a) -> b:(if L.null a then [] else formatting a)) (L.splitAt 4 str)

sample'' = Prelude.putStrLn $ L.unlines $ fmap L.concat $
           (fmap (\i -> printf "%s Nig%sger %d %s " (col [0,1,3,4,36]) (col [i]) i (col [])))
           <$> formatting numbers where
    numbers = L.concat [[8..29 :: Int]]
    formatting str = (\(b,a) -> b:(if L.null a then [] else formatting a)) (L.splitAt 4 str)

maybe0 i = if i /= 0 then Just i else Nothing

mean xs = sum (fromRational . toRational <$> xs) / fromIntegral (L.length xs)

median xs =
    if len `mod` 2 == 0 then
        ((fr $ xs !! (len - 2)) + (fr $ xs !! (len - 1))) / 2
    else
        fr $ xs !! (len - 1) where
            len = L.length xs

index' f = M.fromList . fmap (\e -> (f e, e))

tab' :: ByteString -> String
tab' bs = do
    s <- unpack bs
    case s of
        '\n' -> "\n    "
        i -> return i

showStats :: [(Stat, Int)] -> String
showStats stats = show $ SpacedL $ uncurry (flip Spaced) <$> stats

mapMap :: M.IntMap String
mapMap = M.fromList 
    [ (0,   "Eastern Kingdoms")
    , (1,   "Kalimdor")
    , (33,  "Shadowfang Keep")
    , (34,  "Stormwind Stockade")
    , (36,  "Deadmines")
    , (43,  "Wailing Cavers")
    , (47,  "Razorfen Kraul")
    , (48,  "Blackfathom Deeps")
    , (70,  "Uldaman")
    , (90,  "Gnomeregan")
    , (109, "Sunken Temple")
    , (129, "Razorfen Downs")
    , (189, "Scarlet Monastery")
    , (209, "Zul'Farrak")
    , (229, "Blackrock Spire")
    , (230, "Blackrock Depths")
    , (289, "Scholomance")
    , (309, "Zul'gurub")
    , (329, "Stratholme")
    , (349, "Mauradon")
    , (389, "Ragefire Chasm")
    , (409, "Molten Core")
    , (429, "Dire Maul")
    , (469, "Blackwing Lair")
    , (509, "Ruins of Ahn'Qiraj")
    , (531, "Ahn'Qiraj Temple")
    , (533, "Naxxramas")
    -- Outland
    , (530, "Outland")
    , (542, "Blood Furnace")
    , (543, "Hellfire Ramparts")
    , (546, "The Underbog")
    , (547, "Slave Pens")
    , (550, "Tempest Keep")
    , (552, "The Arcatraz")
    , (553, "The Botanica")
    , (554, "The Mechanar")
    , (555, "Shadow Labyrinth")
    , (556, "Sethekk Halls")
    , (557, "Mana-Tombs")
    , (558, "Auchenai Crypts")
    , (565, "Gruul's Lair")
    -- Northrend
    , (571, "Northrend")
    , (574, "Utgarde Keep")
    , (575, "Utgarde Pinnacle")
    , (576, "The Nexus")
    , (578, "The Oculus")
    , (595, "The Culling of Statholme")
    , (599, "Halls of Lightning")
    , (600, "Drak'Tharon Keep")
    , (601, "Azjol-Nerub")
    , (602, "Halls of Stone")
    , (603, "Ulduar")
    , (604, "Gundrak")
    , (615, "The Obsidian Sanctum")
    , (619, "Ahn'kahet")
    , (624, "Vault of Archavon")
    , (631, "Icecrown Citadel")
    , (632, "The Forge of Souls")
    -- , (
    ]

stormPeaks :: [Point]
stormPeaks = t2p <$> [(5879.771,-215.24387,956.1554,571),(6098.73,-309.07626,1033.2617,571),(6380.0796,-93.29541,1033.2617,571),(6519.3936,173.11037,1041.0745,571),(6872.4204,211.48264,1041.0745,571),(7042.98,341.90317,1041.0745,571),(7383.643,391.32584,1041.0745,571),(7494.8423,300.5522,1044.861,571),(7870.737,244.70448,1210.0536,571),(7995.717,95.41918,1155.2649,571),(8399.121,292.1178,816.8327,571),(10109.414,-496.8919,880.5497,571),(10101.198,-1743.0946,918.5277,571),(8332.117,-3832.8137,1019.3333,571),(7515.497,-4255.583,1039.9886,571),(6837.308,-3922.8682,1039.9886,571),(6615.6313,-2375.7966,1015.41315,571),(6355.4214,-1976.9484,891.37933,571),(6477.197,-1828.3223,906.1684,571),(6176.3706,-1631.8501,482.53943,571),(5978.249,-1505.5609,370.97986,571),(5995.4277,-1265.0936,447.86066,571),(5757.894,-1011.1605,353.74008,571),(5675.1157,-546.0225,485.4135,571),(5879.771,-215.24387,956.1554,571)]

iceCrown :: [Point]
iceCrown = t2p <$> [(6229.356,1173.4674,843.078,571),(6210.9673,1445.904,825.44415,571),(5622.07,1422.7958,796.7249,571),(5490.358,1215.388,793.0393,571),(5435.3145,1134.558,792.893,571),(5216.3804,1409.8123,790.51013,571),(4888.1265,1714.6653,802.1544,571),(5068.6074,1970.6432,916.35376,571),(5464.8086,2354.4397,1073.3969,571),(5616.751,2806.1863,1182.1093,571),(5853.9663,3245.0774,1237.3684,571),(6260.1016,3553.8623,1278.0302,571),(6537.5527,3839.168,1239.0575,571),(6829.5313,4662.7397,1060.0278,571),(6831.7217,5017.37,1011.2848,571),(7899.364,5322.896,874.3871,571),(9309.401,3388.452,886.2196,571),(9082.501,566.34015,993.95844,571),(8371.939,351.7218,1108.8046,571),(8214.63,447.357,1131.7992,571),(7306.335,547.4454,1107.5991,571),(7079.146,366.9306,1014.9451,571),(6611.0605,311.92535,894.74524,571),(6515.851,170.57272,829.44995,571),(6397.4653,50.33114,763.6708,571),(6125.666,-165.11897,581.3501,571),(5910.781,-146.86745,421.0169,571),(5933.379,220.06393,404.283,571),(5937.0093,488.52487,397.69455,571),(6166.8315,624.7575,439.0869,571),(6353.8657,844.3845,484.944,571),(6229.356,1173.4674,843.078,571)]

sholazarBasin :: [Point]
sholazarBasin = t2p <$> [(5272.867,3630.5713,807.2676,571),(5138.153,3733.716,381.34348,571),(4903.665,3926.5679,387.4961,571),(4751.9595,4068.6462,392.1313,571),(4576.6763,4380.032,418.76276,571),(4573.6245,4702.458,326.95963,571),(4644.831,5000.9814,367.58652,571),(4637.8003,5189.4673,388.12335,571),(4643.9814,5365.4453,407.14902,571),(4644.5303,5472.6074,418.27792,571),(4681.4233,5535.6357,425.6787,571),(4713.876,5613.5874,431.07562,571),(4738.9214,5732.2773,468.01132,571),(4740.7065,5903.8677,485.8968,571),(4711.6855,5981.275,493.95963,571),(4714.6807,6058.3477,496.001,571),(4702.609,6195.3896,494.30704,571),(5094.2363,6381.9707,461.91238,571),(5739.1284,6292.494,447.963,571),(6147.5483,6191.185,456.70532,571),(6147.5483,6191.185,456.70532,571),(6447.781,5945.5425,477.34088,571),(6478.9497,5645.809,490.24756,571),(6710.0215,5528.069,508.3835,571),(6952.702,5340.225,520.81476,571),(6923.6294,5176.74,573.23206,571),(6882.7334,5170.3384,575.19305,571),(6824.645,5124.5483,575.1109,571),(6799.3237,5057.9897,577.091,571),(6797.2744,4917.551,582.6763,571),(6795.9434,4809.4077,589.1096,571),(6776.6772,4674.303,596.807,571),(6822.2817,4563.4326,600.4563,571),(6788.304,4285.1904,619.1112,571),(6658.212,4146.76,622.9513,571),(6608.56,4038.1467,628.55347,571),(6559.2783,3925.2148,646.5923,571),(6461.6255,3816.766,666.8619,571),(6461.6255,3816.766,666.8619,571),(6395.8174,3727.0977,680.80115,571),(6366.5747,3667.5203,687.13306,571),(6263.714,3635.8901,694.9838,571),(6201.137,3601.025,699.62067,571),(6116.8555,3518.3914,697.56696,571),(6031.4004,3415.5986,694.95306,571),(5919.386,3369.6194,690.0035,571),(5823.0747,3368.579,686.2301,571),(5747.9727,3373.4868,683.4626,571),(5677.9067,3373.9185,680.8556,571),(5586.2554,3370.513,672.7301,571),(5521.5313,3404.7793,669.4573,571),(5441.4624,3462.5054,711.8704,571),(5362.768,3480.0764,743.1818,571),(5291.2036,3569.1025,783.6935,571),(5272.867,3630.5713,807.2676,571)]

howlingFjord :: [Point]
howlingFjord = t2p <$> [(3063.3418,-4663.5327,436.22266,571),(3048.935,-4796.179,556.3129,571),(3058.0205,-4917.627,569.6216,571),(3064.0732,-5056.1274,746.39044,571),(3064.8618,-5152.916,745.81433,571),(3061.1946,-5245.9434,739.3776,571),(3070.3862,-5406.9175,728.384,571),(3044.7024,-5599.371,717.4029,571),(3071.4412,-5746.5586,708.27014,571),(2749.4492,-6043.027,679.88934,571),(1374.3488,-6535.87,593.70593,571),(291.09192,-6189.0527,524.9934,571),(-414.72333,-5671.769,474.036,571),(-535.1213,-5098.65,440.5731,571),(-806.6159,-4439.571,398.09766,571),(-897.70953,-2964.2,310.47122,571),(1348.5486,-1503.6583,163.88239,571),(2578.603,-2605.2585,156.79626,571),(2680.9546,-2841.773,128.68596,571),(2660.91,-2850.6538,128.68596,571),(2781.0166,-2967.655,163.64581,571),(2930.61,-3218.1296,237.30449,571),(3079.1892,-3389.1594,504.99677,571),(3047.145,-3899.6672,819.9882,571),(3063.3325,-4632.633,664.85596,571),(3063.3418,-4663.5327,436.22266,571)]

grizzlyHills :: [Point]
grizzlyHills = t2p <$> [(3068.0227,-4657.3604,662.3183,571),(3069.4937,-4987.981,685.39276,571),(3086.52,-5313.508,738.1847,571),(3072.4277,-5564.254,722.8788,571),(3071.206,-5898.641,688.5791,571),(3250.7297,-6217.1924,673.34937,571),(4950.844,-5757.9424,592.5461,571),(5784.0347,-4846.469,436.31247,571),(5470.9043,-4602.357,405.81766,571),(5226.373,-4552.349,434.91785,571),(5073.688,-4319.157,403.53928,571),(4924.927,-3987.8809,395.01056,571),(4983.5327,-3841.6582,437.20786,571),(4906.089,-3722.362,414.92062,571),(4664.6396,-3458.5063,351.21338,571),(4477.6313,-3190.935,348.99976,571),(4456.1577,-2993.1196,363.13934,571),(4410.349,-2698.037,356.82397,571),(4526.1,-2434.6973,310.7996,571),(4640.895,-2251.888,295.14612,571),(4629.3276,-1803.4443,269.34726,571),(4417.7944,-1606.6399,261.43546,571),(4197.574,-1519.4414,247.30193,571),(4034.0762,-1533.6866,233.17073,571),(3843.0725,-1625.7449,211.71106,571),(3428.8176,-1659.1187,180.81659,571),(3045.035,-1572.7052,143.1413,571),(2700.3193,-1542.9407,76.50675,571),(1985.1315,-1663.8694,108.490746,571),(2600.1953,-2562.436,88.59495,571),(2685.1606,-2745.6987,95.67966,571),(2685.2595,-2927.1858,126.992485,571),(2814.5452,-2956.2944,133.96973,571),(2904.2124,-2995.6177,134.27312,571),(3004.798,-3021.4048,144.17102,571),(3022.6802,-3299.2405,322.9672,571),(3068.866,-3750.0535,646.56323,571),(3073.0737,-4209.377,766.5647,571),(3069.4421,-4608.7407,755.25006,571),(3068.0227,-4657.3604,662.3183,571)]

zulFarrak :: [Point]
zulFarrak = t2p <$> [(4649.288,-3399.8171,873.3059,571),(4802.955,-3600.576,789.9725,571),(4930.1284,-3730.8357,734.80457,571),(5013.275,-3806.7146,701.2011,571),(5015.0415,-4124.6577,644.70844,571),(5179.75,-4307.169,612.33606,571),(5174.8506,-4395.318,602.7082,571),(5256.3657,-4558.4624,607.2793,571),(5454.0415,-4561.855,614.5005,571),(5534.5547,-4766.36,621.32513,571),(6964.537,-5533.4814,717.085,571),(7855.861,-4688.837,686.47186,571),(7236.286,-4291.8647,517.63086,571),(7049.6826,-4038.2908,531.6804,571),(6834.291,-4037.891,570.3664,571),(6651.5435,-4038.6802,569.7073,571),(6609.934,-4003.5732,567.4297,571),(6627.0127,-3586.7844,551.10425,571),(6630.1577,-3020.482,494.61392,571),(6504.687,-2254.7107,446.85034,571),(6323.288,-2013.9615,500.5907,571),(6198.7603,-1827.3978,502.9773,571),(6056.261,-1619.8793,417.2913,571),(5920.6865,-1253.4534,363.5053,571),(5721.5605,-1236.4331,341.1437,571),(5452.045,-1206.9014,338.6171,571),(5036.0996,-1106.0901,371.811,571),(5036.6836,-1324.6426,344.56693,571),(4909.5234,-1481.2438,349.85693,571),(4738.931,-1621.0864,333.326,571),(4711.808,-2005.1936,497.15503,571),(4699.815,-2348.0698,437.8928,571),(4545.5825,-2434.899,405.98724,571),(4439.918,-2707.3633,431.05865,571),(4474.1567,-3068.7295,402.50012,571),(4606.358,-3316.1343,369.28082,571),(4649.288,-3399.8171,873.3059,571)]

dragonblight :: [Point]
dragonblight = t2p <$> [(3183.887,-1559.562,109.62236,571),(3618.0684,-1631.6658,127.52534,571),(3940.9617,-1564.6113,155.48604,571),(4060.3315,-1486.8214,159.81735,571),(4378.244,-1575.2874,179.38623,571),(4436.3394,-1596.0299,178.93245,571),(4662.4785,-1782.7213,275.04642,571),(4743.721,-1571.5535,321.61005,571),(4965.5825,-1397.4547,309.03772,571),(5032.371,-1279.0154,368.47388,571),(5024.743,-1097.1676,456.65887,571),(4852.607,-967.0865,472.7061,571),(4753.4873,-837.1618,613.86786,571),(4830.6714,-556.1178,614.9073,571),(4871.9233,-386.75934,584.4551,571),(5024.189,-187.98349,518.78217,571),(5092.342,-12.2186575,458.70605,571),(5194.726,134.18561,425.41953,571),(5193.6494,308.07303,391.3749,571),(5182.2427,460.25064,398.68213,571),(5057.6445,564.7754,404.67545,571),(5096.051,819.5597,408.46494,571),(5191.265,969.8712,388.59357,571),(5191.265,969.8712,388.59357,571),(5357.1084,1134.7496,366.872,571),(5224.1436,1245.6605,526.3178,571),(5225.273,1360.1537,540.90125,571),(4946.2954,1652.6849,529.05865,571),(4872.4277,1700.4359,583.5687,571),(4654.4937,1832.1898,553.1737,571),(4588.9395,1764.5393,531.66833,571),(4445.007,1695.3474,434.58585,571),(4277.832,1695.9669,414.41824,571),(4217.0127,1728.993,395.5745,571),(4120.531,1832.8446,386.35385,571),(3952.2249,1926.2618,452.91507,571),(3927.7117,2020.8184,428.88962,571),(3966.5464,2073.2444,406.50607,571),(4166.115,2022.8713,372.24738,571),(4228.7217,2158.4954,373.36304,571),(4215.3066,2329.1406,363.41922,571),(4066.8923,2431.1152,373.6399,571),(3946.7517,2395.7476,366.51712,571),(3966.3062,2531.417,358.68835,571),(4029.4067,2775.7292,406.0731,571),(4028.035,2850.7693,386.93747,571),(4060.437,3007.8218,370.60776,571),(4060.6475,3091.0488,378.1356,571),(3933.7422,3198.8672,436.3367,571),(3826.944,3199.6233,372.04562,571),(3708.1138,3093.354,307.75113,571),(3483.4312,3149.4717,262.37238,571),(3292.379,3250.7024,226.86108,571),(2947.1243,3180.8855,184.96542,571),(1920.3065,1906.2544,194.7801,571),(2043.9836,-1255.1304,231.35199,571),(2739.577,-1539.7524,142.0165,571),(2853.2004,-1586.5137,105.955986,571),(3183.887,-1559.562,109.62236,571)]

boreanTundra :: [Point]
boreanTundra = t2p <$> [(4697.1304,5569.94,95.860054,571),(4727.757,5677.1074,136.9532,571),(4728.4824,5794.0947,108.40435,571),(4728.6587,5903.077,93.898026,571),(4699.765,5959.877,89.00512,571),(4675.312,6019.4023,86.85491,571),(4724.8784,6369.5566,77.29587,571),(4804.9443,7785.1333,213.07326,571),(3751.713,8035.5444,234.81953,571),(1195.4526,7181.922,312.07886,571),(785.4813,5451.832,409.78387,571),(2560.9346,3148.0393,513.5449,571),(3250.9248,3279.6785,486.8456,571),(3388.6648,3217.009,467.01965,571),(3520.0679,3171.2644,464.03287,571),(3621.7751,3106.2034,463.74405,571),(3805.3076,3205.0762,448.9265,571),(3997.6333,3238.0232,383.37662,571),(4198.612,3433.8298,371.98895,571),(4226.1997,3655.0466,396.74872,571),(4197.7407,3837.663,433.25012,571),(4195.902,3943.919,480.35547,571),(4223.896,4035.3096,467.27502,571),(4262.481,4201.7773,458.8795,571),(4457.4453,4321.3804,440.75485,571),(4544.128,4379.038,424.502,571),(4553.9287,4531.623,352.0022,571),(4553.3696,4789.179,272.56567,571),(4595.693,4973.1123,216.93767,571),(4626.865,5183.8936,166.177,571),(4622.1494,5341.7637,137.84966,571),(4628.9844,5434.0957,99.66674,571),(4632.7935,5467.842,121.79921,571),(4697.1304,5569.94,95.860054,571)]
