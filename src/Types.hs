{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.ByteString.Char8 hiding (foldl1)

data Slot = Head | Neck | Shoulders | Back | Chest | Waist | Pants | Wrists
    | Hands | Feet | Finger | Trinket | MainHand | OffHand | AnyHand | TwoHands
    | UnknownSlot Int
    deriving (Show, Read)

data Stat = Mana | HP | Agility | Strength | Intellect | Spirit | Stamina 
    | Defense | Dodge | Parry | Block | Hit | Crit | Resilence | Haste 
    | Expertise | AttackPower | HealingPower | ManaPer5 | SpellPower 
    | UnknownStat Int
    deriving (Show, Read)

data Class = Mage | Priest | Warlock | Druid | Rogue | Hunter | Shaman
    | DeathKnight | Paladin | Warrior
    deriving (Show, Read)

data Faction = Alliance | Horde
    deriving (Show, Read)

type Level = Int
type ItemLevel = Level

data Requirements = Requirements
    { r_lvl   :: Maybe Level
    , r_fac   :: Maybe Faction
    , r_class :: Maybe Class
    } deriving Show

noRequirements :: Requirements
noRequirements = Requirements Nothing Nothing Nothing

data Item = Item 
    { iid     :: Int
    , iname   :: ByteString 
    , islot   :: Slot
    , istats  :: [(Stat,Int)] 
    , ilevel  :: ItemLevel 
    , ispells :: [ByteString]
    , ireq    :: Requirements
    }

-- turban = Item 12837 "w/e turban" Head [(Intellect, 10), (Hit, 8), (SpellPower, 32)] 62 (Just 58)

class Abr a where
    abr :: a -> String
              
instance Abr Stat where
    abr a = case a of
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
        Resilence -> "res"
        AttackPower   -> "atk"
        HealingPower  -> "heal"
        SpellPower    -> "sp"
        _         -> "???"

instance Show Item where
    show i = (unpack $ iname i) ++ " (" ++ 
             foldl1 (\a b -> a ++ ", " ++ b) ((\(s,a) -> show a ++ " " ++ abr s) <$> (istats i))
