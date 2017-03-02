{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, PolyKinds #-}

module Query where

import qualified Data.ByteString as B
import qualified Data.IntMap as M
import qualified Data.Map as Ma
import qualified Data.List as L

import System.Process (callCommand)

import Source
import Types
import Raw_items

type C a b c = (b -> c) -> a -> c

class Filterable f where
    filter' :: (a -> Bool) -> f a -> f a

class Filterable2 f where
    filter'' :: (a -> b -> Bool) -> f a b -> f a b

instance Filterable [] where
    filter' = L.filter

instance Filterable M.IntMap where
    filter' = M.filter

instance Filterable2 Ma.Map where
    filter'' = Ma.filterWithKey

class Listable f where
    toList' :: f a -> [a]

instance Listable [] where
    toList' = id

instance Listable M.IntMap where
    toList' = M.elems

mkC :: (a -> b) -> C a b c
mkC = flip (.)

filters :: Filterable f => [a -> Bool] -> f a -> f a
filters con = filter' (\e -> L.all ($ e) con)

sorts :: (Listable f, Ord b) => C a b b -> f a -> [a]
sorts f = L.reverse . L.sortOn (f id) . toList'

takes = L.take

like :: B.ByteString -> B.ByteString -> Bool
like = B.isInfixOf

by_iid = mkC iid
by_iname = mkC iname
by_islot = mkC islot
by_iatype = mkC iatype
by_istats = mkC istats
by_ilevel = mkC ilevel
by_iqual = mkC iqual
by_irlevel = mkC irlevel
by_idesc = mkC idesc
by_score tab = mkC (\i -> score tab (istats i) 
               (su_stats <$> L.filter (\s -> su_chance s > 2) (isuffs i)))

by_sid = mkC sid
by_sval = mkC sval
by_stype = mkC stype
by_sdesc = mkC sdesc

score :: [(Stat,Double)] -> [(Stat,Int)] -> [[(Stat,Int)]] -> Double
score tab hay [] = L.sum $ do
    (s,v) <- tab
    (s', v') <- hay
    if s == s' then
        return (v * fromIntegral v')
    else
        []
score tab hay opt = score tab hay [] + maximum ((\l -> score tab l []) <$> opt)

std n scoretab filts = takes n $ sorts (by_score scoretab) $ filters filts raw_items

dmg = [(Damage, 1.0)]

by_speed :: C Item Double Double
by_speed = mkC $ \i -> L.sum $ do
    (s,n) <- istats i
    if s == Speed && n > 0 then
        return $ 1 / (fromIntegral n)
    else
        return 0

defScore = [ (Strength, 0.5), (Defense, 1), (Stamina, 0.5), (Dodge, 0.5)
           , (Parry, 0.5), (Block, 0.5), (Agility, 0.5), (AttackPower, 0.25) ]

armsScore = [ (Strength, 1), (AttackPower, 0.5), (Crit, 0.5), (Haste, 0.2)
            , (Hit, 1.5), (Expertise, 1), (Agility, 0.5) ]

warrScore = [ (Strength, 1), (AttackPower, 0.5), (Crit, 0.5), (Haste, 0.2)
            , (Hit, 0.5), (Expertise, 0.5), (Defense, 1), (Parry, 0.5)
            , (Dodge, 0.5), (Block, 1) ]

furyScore = [ (Strength, 1), (AttackPower, 0.5), (Crit, 0.75), (Haste, 0.5)
            , (Hit, 1), (Expertise, 1), (Agility, 0.75) ]

hunterScore = [ (Agility, 1), (Hit, 0.75), (Intellect, 0.7), (AttackPower, 0.5)
              , (Crit, 0.5), (Expertise, 1), (Strength, 0.5) ]

rogueScore = [ (Agility, 1), (Hit, 1.5), (AttackPower, 0.6), (Crit, 0.4)
             , (Expertise, 1.5), (Strength, 0.6) ]

warlockScore = [ (SpellPower, 1), (Hit, 2), (Intellect, 0.2), (Crit, 0.3)
               , (Haste, 0.5), (SpellPen, 1) ]

intScore = [ (SpellPower, 1), (Hit, 2), (Intellect, 0.75), (Crit, 0.5)
           , (Haste, 0.5), (SpellPen, 1) ]

spScore = [ (SpellPower, 1), (Hit, 0.5), (Intellect, 0.5), (Crit, 0.5)
          , (Haste, 0.5), (SpellPen, 1) ]

mp5Score = [ (Spirit, 2), (ManaPer5, 1), (Intellect, 0.1) ]

goitem id = callCommand $ "xdg-open http://truewow.org/armory/item.php?item=" ++ show id
