{-# LANGUAGE  OverloadedStrings
            , StandaloneDeriving
            , DeriveGeneric
            , FlexibleInstances #-}

module Types where

import Control.DeepSeq
import Control.Monad 

import Data.ByteString.Char8 hiding (foldl1)
import qualified Data.ByteString.Char8 as B
import Data.Int
import qualified Data.IntMap as M
import qualified Data.List as L
import qualified Data.Map as Ma
import Data.Maybe
import qualified Data.Vector as V
import Data.Serialize
import Data.Word

import GHC.Generics

import Text.Printf (printf)

import DBC

data Mappings = Mappings
    { m_spells    :: M.IntMap Spell
    , m_quests    :: M.IntMap Quest
    , m_i2s       :: M.IntMap [(SuffixId, Float)]
    , m_i2p       :: M.IntMap [(PropertyId, Float)]
    , m_suffixes  :: M.IntMap SuffixEntry
    , m_props     :: M.IntMap PropertyEntry
    , m_enchs     :: M.IntMap EnchantmentEntry
    , m_sufmap    :: SuffixMap
    }


data Slot = Head | Neck | Shoulder | Back | Chest | Waist | Legs | Wrists
    | Hands | Feet | Finger | Trinket | MainHand | OffHand | Weapon | TwoHand
    | Ranged | Bag | HandHeld | Thrown | Ammo | Relic | Quiver | Shirt | Tabard 
    | Shield | UnknownSlot Int
    deriving (Show, Read, Eq, Generic, Ord)

instance Serialize Slot
instance NFData Slot

isWeapon :: Slot -> Bool
isWeapon slot = L.any (== slot) [MainHand, OffHand, Weapon, TwoHand, Ranged]

data ArmorType = NotArmor | Cloth | Leather | Mail | Plate
    deriving (Eq, Ord, Generic)

instance Serialize ArmorType
instance NFData ArmorType

instance Show ArmorType where
    show NotArmor = ""
    show Cloth = "Cloth"
    show Leather = "Leather"
    show Mail = "Mail"
    show Plate = "Plate"

instance Enum ArmorType where
    fromEnum = undefined
    toEnum n = case n of
        5 -> Mail
        6 -> Plate
        7 -> Cloth
        8 -> Leather
        _ -> NotArmor

appendAT :: String -> ArmorType -> String
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
    deriving (Eq, Read, Generic, Ord)

instance Serialize Stat
instance NFData Stat

data Quality = Poor | Common | Uncommon | Rare | Epic | Legendary
    | Artifact | BtA
    deriving (Eq, Ord, Show, Read, Generic)

qualc :: Item -> String
qualc i = col $ case iqual i of
    Poor      -> []
    Common    -> [0,1]
    Uncommon  -> [1,32]
    Rare      -> [1,34]
    Epic      -> [1,35]
    Legendary -> [1,33]
    _         -> [1,36]

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
        7 -> BtA

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

type Level = Int
type ItemLevel = Level

data Item = Item 
    { iid     :: Int
    , iname   :: ByteString 
    , islot   :: Slot
    , iatype  :: ArmorType
    , istats  :: [(Stat,Int)] 
    , isuffs  :: [Suffix]
    , ilevel  :: ItemLevel 
    , iqual   :: Quality
    , irlevel :: Level
    , idesc   :: ByteString
    } deriving Generic

instance Serialize Item
instance NFData Item

instance Show Item where
    show i = printf "%s#%s%-5d %s (%s) %s%s%s%s%s\n" 
        (col [1,36]) (col [0,36]) (iid i) (qualc i ++ (unpack $ iname i) ++ col [])
        (appendAT (show $ islot i) (iatype i)) 
        (showStats $ istats i) (col [32]) (tab $ idesc i) (col [])
        (foldMap ("\n        " ++) (show <$> isuffs i))
        
type SpellId = Word32

data Spell = Spell
    { sid     :: SpellId
    , sval    :: Int32
    , stype   :: (Word32, Word32)
    , sdesc   :: ByteString
    } deriving (Show, Generic)

instance Serialize Spell where

instance Gettable Spell where
    get' strs = do
        id <- get' strs
        skip (4*79)
        n <- get' strs
        skip (4*14)
        t1 <- get' strs
        skip (4*14) 
        t2 <- get' strs
        skip (4*59)
        desc <- get' strs
        return $ Spell id (n+1) (t1,t2) (replaceSubstring "$s1" (pack $ show (n+1)) desc)

data Quest = Quest 
    { qid     :: Int
    , qname   :: ByteString
    , qlevel  :: Level
    , qfac    :: Maybe Faction
    , qitems  :: [Int]
    } deriving (Show, Generic)

instance Serialize Quest where

type SuffixId = Word32
data SuffixEntry = SuffixEntry
    { se_id     :: SuffixId
    , se_suffix :: ByteString
    , se_enchs  :: [EnchantmentId]
    } deriving (Show, Generic)

instance Serialize SuffixEntry where

instance Gettable SuffixEntry where
    get' strs = do
        id <- get' strs
        suffix <- get' strs
        skip (4*17)
        es <- replicateM 5 (get' strs)
        return $ SuffixEntry id suffix (L.filter (/=0) es)

type PropertyId = Word32
data PropertyEntry = PropertyEntry 
    { pe_id     :: PropertyId
    , pe_suffix :: ByteString
    , pe_enchs  :: [EnchantmentId]
    } deriving (Show, Generic)

instance Serialize PropertyEntry where

instance Gettable PropertyEntry where
    get' strs = do
        id <- get' strs
        suffix <- get' strs
        es <- replicateM 3 (get' strs)
        return $ PropertyEntry id suffix (L.filter (/= 0) es)

type EnchantmentId = Word32
data EnchantmentEntry = EnchantmentEntry
    { ee_id     :: EnchantmentId
    , ee_stats  :: [(Stat,Int)]
    , ee_spells :: [SpellId]
    } deriving (Show, Generic)

instance Serialize EnchantmentEntry where

instance Gettable EnchantmentEntry where
    get' strs = do
        id <- get' strs
        skip 4
        ts <- replicateM 3 getWord32le
        ns <- replicateM 3 $ fromIntegral <$> getWord32le
        skip (3*4)
        ss <- replicateM 3 getWord32le
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
        return $ EnchantmentEntry id stats spells

type SuffixMap = M.IntMap [Suffix]

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

data Point = Point
    { p_x :: Float
    , p_y :: Float
    , p_z :: Float
    , p_m :: Int
    } deriving Generic

instance Serialize Point

instance Show Point where
    show p = printf "%s(%s%.3f%s,%s %.3f%s,%s %.3f%s)%s %s%-3d%s %s%s%s" 
        (col [1]) (col []) (p_x p) (col [1]) (col []) (p_y p) (col [1]) (col []) 
        (p_z p) (col [1]) (col [])  (col [32]) (p_m p) (col [38]) (col [1])
        (maybe "<unknown>" id $ M.lookup (p_m p) mapMap) (col [])

type GameObjects = [GameObject]

data GameObject = GameObject
    { gid     :: Int
    , gname   :: ByteString
    , gpoint  :: Point
    } deriving Generic

instance Eq GameObject where
    go == go' = gid go == gid go'

instance Ord GameObject where
    compare go go' = compare (gid go) (gid go')

instance Serialize GameObject

instance Show GameObject where
    show go = printf "%s#%s%d%s %s %s" (col [1,36]) (col [0,36]) 
              (gid go) (col []) (unpack $ gname go) (show $ gpoint go)

getSpellStats :: Spell -> Maybe (Stat,Int)
getSpellStats sp = (\i -> (i, fromIntegral $ sval sp)) <$> case (stype sp) of
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

instance Show Stat where
    show a = (\s -> "\ESC[1m\STX" ++ s ++ "\ESC[21m\STX") $ case a of
        Mana      -> "mana"
        HP        -> "hp"
        Agility   -> "agi"
        Strength  -> "str"
        Intellect -> "int"
        Spirit    -> "spi"
        Stamina   -> "sta"
        Defense   -> "def"
        Dodge     -> "dge"
        Parry     -> "par"
        Block     -> "blk"
        Crit      -> "crit"
        Haste     -> "hast"
        Hit       -> "hit"
        Expertise -> "exp"
        ManaPer5  -> "mp5"
        Resilence -> "resil"
        AttackPower   -> "attk"
        HealingPower  -> "heal"
        SpellPower    -> "sp"
        BlockValue    -> "blkv"
        Speed     -> "delay"
        Damage    -> "dmg"
        HolyRes   -> "holy-r"
        ShadowRes -> "shad-r"
        FireRes   -> "fire-r"
        FrostRes  -> "fros-r"
        NatureRes -> "nat-r"
        ArcaneRes -> "arc-r"
        ArmorPen  -> "arpen"
        SpellPen  -> "spen"
        Vitality  -> "hp5"
        UnknownStat n -> col [7] ++ "?? " ++ show n

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

replaceSubstring :: ByteString -> ByteString -> ByteString -> ByteString
replaceSubstring needle rep hay =
    case breakSubstring needle hay of
        (l,"") -> l
        (l,r) -> l `B.append` rep `B.append` (replaceSubstring needle rep $ B.drop (B.length needle) r) 

--
-- utils
--

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

tab :: ByteString -> String
tab bs = do
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
