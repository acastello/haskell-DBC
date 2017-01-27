{-# LANGUAGE  OverloadedStrings
            , StandaloneDeriving
            , DeriveGeneric
            , FlexibleInstances #-}

module Types where

import Data.ByteString.Char8 hiding (foldl1)
import qualified Data.ByteString.Char8 as B
import Data.Int
import qualified Data.List as L
import Data.Maybe
import qualified Data.Vector as V
import Data.Serialize
import Data.Word

import GHC.Generics

import DBC

data Slot = Head | Neck | Shoulder | Back | Chest | Waist | Legs | Wrists
    | Hands | Feet | Finger | Trinket | MainHand | OffHand | Weapon | TwoHand
    | Ranged | Bag | HandHeld | Thrown | Ammo | Relic | Quiver | Shirt | Tabard 
    | Shield | UnknownSlot Int
    deriving (Show, Read, Eq, Generic, Ord)

instance Serialize Slot

data Stat = Mana | HP | Agility | Strength | Intellect | Spirit | Stamina 
    | Armor | Defense | Dodge | Parry | Block | Hit | Crit | Resilence | Haste 
    | Expertise | AttackPower | HealingPower | ManaPer5 | SpellPower | ArmorPen
    | Vitality | SpellPen | BlockValue
    | HolyRes | ShadowRes | FireRes | FrostRes | NatureRes | ArcaneRes 
    | UnknownStat Int
    deriving (Read, Generic)

instance Serialize Stat

data Class = Mage | Priest | Warlock | Druid | Rogue | Hunter | Shaman
    | DeathKnight | Paladin | Warrior
    deriving (Show, Read, Generic)

instance Serialize Class

data Faction = Alliance | Horde
    deriving (Show, Read, Generic)

instance Serialize Faction

type Level = Int
type ItemLevel = Level

data Requirements = Requirements
    { r_lvl   :: Maybe Level
    , r_fac   :: Maybe Faction
    , r_class :: Maybe Class
    } deriving (Generic, Show)

instance Serialize Requirements

noRequirements :: Requirements
noRequirements = Requirements Nothing Nothing Nothing

data Item = Item 
    { iid     :: Int
    , iname   :: ByteString 
    , islot   :: Slot
    , istats  :: [(Stat,Int)] 
    , ilevel  :: ItemLevel 
    , ispells :: [Word32]
    , ireq    :: Requirements
    , idesc   :: ByteString
    } deriving Generic

instance Serialize Item

instance Show Item where
    show i = "#" ++ show (iid i) ++ " " ++ 
             (unpack $ iname i) ++ " (" ++ 
             show (islot i) ++ ") " ++
             (show $ SpacedL $(uncurry (flip Spaced) <$> istats i)) ++ 
             tab (idesc i) ++ "\n"

tab :: ByteString -> String
tab bs = do
    s <- unpack bs
    case s of
        '\n' -> "\n\t"
        i -> return i

data Spell = Spell
    { sid     :: Word32
    , sval    :: Int32
    , stype   :: (Word32, Word32)
    , sdesc   :: ByteString
    } deriving (Show, Generic)

instance Serialize Spell

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

getSpellStats :: Spell -> Maybe (Stat,Int)
getSpellStats sp = (\i -> (i, fromIntegral $ sval sp)) <$> case (stype sp) of
    (135,126) -> Just SpellPower
    (13,126)  -> Just SpellPower
    (99,0)    -> Just AttackPower
    (85,0)    -> Just ManaPer5
    (123,124) -> Just SpellPen
    (158,0)   -> Just BlockValue
    (161,0)   -> Just Vitality
    (189,2)   -> Just Defense
    (189,4)   -> Just Parry
    (189,8)   -> Just Dodge
    (189,224) -> Just Hit
    (189,1782)-> Just Crit
    (189,1792)-> Just Crit
    (189,16777216) -> Just ArmorPen
    _ -> Nothing

getSpellItems :: Spell -> [Item] -> [Item]
getSpellItems sp is = L.filter (\s -> L.any (== sid sp) (ispells s)) is

instance Show Stat where
    show a = case a of
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
        HolyRes   -> "holy-r"
        ShadowRes -> "shad-r"
        FireRes   -> "fire-r"
        FrostRes  -> "fros-r"
        NatureRes -> "nat-r"
        ArcaneRes -> "arc-r"
        ArmorPen  -> "arpen"
        SpellPen  -> "spen"
        Vitality  -> "hp5"
        UnknownStat n -> "?? " ++ show n

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
