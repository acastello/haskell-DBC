{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification #-}

module Query where

import qualified Data.IntMap as M
import qualified Data.List as L

import Source
import Types
import Raw_items

type C a b c = (b -> c) -> a -> c

class Filterable f where
    filter' :: (a -> Bool) -> f a -> f a

instance Filterable [] where
    filter' = L.filter

instance Filterable M.IntMap where
    filter' = M.filter

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
sorts f = L.sortOn (f id) . toList'

by_iid = mkC iid
by_iname = mkC iname
by_islot = mkC islot
by_iatype = mkC iatype
by_istats = mkC istats
by_ilevel = mkC ilevel
by_iqual = mkC iqual
by_irlevel = mkC irlevel
by_idesc = mkC idesc
by_score tab = mkC (score tab . istats)

score :: [(Stat,Double)] -> [(Stat,Int)] -> Double
score tab hay = L.sum $ do
    (s,v) <- tab
    (s', v') <- hay
    if s == s' then
        return (v * fromIntegral v')
    else
        []

defScore = [ (Strength, 1), (Defense, 1), (Dodge, 0.5), (Parry, 0.5)
           , (Block, 0.5), (Agility, 0.5), (AttackPower, 0.5) ]

armsScore = [ (Strength, 1), (AttackPower, 0.5), (Crit, 0.5), (Haste, 0.2)
            , (Hit, 1.5), (Expertise, 1), (Agility, 0.5) ]

hunterScore = [ (Agility, 1), (Hit, 1), (Intellect, 0.7), (AttackPower, 0.5)
              , (Crit, 0.5), (Expertise, 1), (Strength, 0.5) ]

warlockScore = [ (SpellPower, 1), (Hit, 2), (Intellect, 0.2), (Crit, 0.3)
               , (Haste, 0.5), (SpellPen, 1) ]

intScore = [ (SpellPower, 1), (Hit, 2), (Intellect, 0.75), (Crit, 0.5)
           , (Haste, 0.5), (SpellPen, 1) ]

spScore = [ (SpellPower, 1), (Hit, 0.5), (Intellect, 0.5), (Crit, 0.5)
          , (Haste, 0.5), (SpellPen, 1) ]

rogueScore = [ (Agility, 1), (Hit, 1), (AttackPower, 0.5), (Crit, 0.5)
             , (Haste, 0.5), (Expertise, 1)]


