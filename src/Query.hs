{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Query where

import Control.Concurrent

import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as M
import qualified Data.Map as Ma
import qualified Data.List as L
import Data.Word

import System.IO.Unsafe
import System.Process (callCommand)

import Source
import Types
import Geometry
import Raw_items
import OpenGL

type C a b c = (b -> c) -> a -> c

class Filterable (f :: * -> *) where
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
sorts f = L.reverse . L.sortOn (f id) . toList'

takes = L.take

groups :: (Listable f, Eq b) => C a b b -> f a -> [[a]]
groups f = L.groupBy (\a b -> f id a == f id b) . toList'

like :: B.ByteString -> B.ByteString -> Bool
like = B.isInfixOf

dist :: Point -> Point -> Float
dist p0 p1 = 
    if p_m p0 /= p_m p1 then
        1/0
    else
        sqrt $ sum $ fmap (**2) $ 
          zipWith subtract [p_x p0, p_y p0, p_z p0] [p_x p1, p_y p1, p_z p1]


-- item getters
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

-- suffix getters
by_sid = mkC sid
by_sval = mkC sval
by_stype = mkC stype
by_sdesc = mkC sdesc

-- GameObject getters
by_gid    = mkC gid
by_gname  = mkC gname
by_g_p    = mkC gpoint
by_p_m    = mkC (p_m . gpoint)
by_dist p = mkC (dist p . gpoint)

-- generic comparing functions
is_instance n = not $ any (== n) [0, 1, 530, 571]

is_herb id = any (== id)
  [ 1617,   1618,   1629,   1620,   1621,   1622,   1623,   1624,   1625,   1628 
  , 2041,   2042,   2043,   2044,   2045,   2046,   2866,   3724,   3726
  , 3727,   3729,   3730,   3725,   142140, 142141, 142142, 142143, 142144 
  , 142145, 176583, 176584, 176586, 176587, 176588, 176589, 176636, 176637
  , 176638, 176639, 176640, 176641, 176642, 180164, 180165, 180166, 180167 
  , 180168, 181166, 181270, 181271, 181275, 181277, 181278, 181279
  , 181280, 181281, 183043, 183044, 183045, 183046, 185881, 189973, 190169
  , 190170, 190171, 190172, 190173, 190174, 190175, 190176, 191019, 191303 ]

is_vein id = any (== id) 
  [ 181556, 185557, 189978, 1731, 3763, 2055, 181248, 103713, 165658, 181555
  , 1734, 150080, 181109, 180215, 1610, 1667, 19903, 1735, 181557, 181069
  , 2653, 2040, 150079, 176645, 185877, 73941, 123310, 177388, 73940, 123848
  , 123309, 181569, 181570, 189979, 189981, 175404, 189980, 1733, 105569
  , 181068, 324, 150082, 176643, 1732, 3764, 2054, 181249, 103711, 191133
  , 2047, 181108, 150081]

score :: [(Stat,Double)] -> [(Stat,Int)] -> [[(Stat,Int)]] -> Double
score tab hay [] = L.sum $ do
    (s,v) <- tab
    (s', v') <- hay
    if s == s' then
        return (v * fromIntegral v')
    else
        []
score tab hay opt = score tab hay [] + maximum ((\l -> score tab l []) <$> opt)

gos :: GameObjects
gos = unsafePerformIO loadGameObjects

std :: Int -> [(Stat, Double)] -> [Item -> Bool] -> [Item]
std n scoretab filts = takes n $ sorts (by_score scoretab) $ filters filts raw_items

std' xs = loadGameObjects >>= \gos -> foldMap print $ filters xs gos

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

-- util
instance Bidimensional Point where
    vert Point { p_x = x, p_y = y } = vertex $ Vertex2 (-y) x
    bounds Point { p_x = x, p_y = y } 
                         = bounds (realToFrac (-y) :: Double, realToFrac x :: Double)

p2tuple :: Point -> (Float, Float, Float, Word32)
p2tuple p = (p_x p, p_y p, p_z p, fromIntegral $ p_m p)

saveGO :: GameObjects -> IO ()
saveGO [] = error "no gameobjects"
saveGO xs = save ("tup4_" ++ B.unpack (gname $ head xs) ++ ".gz") 
                  ((p2tuple . gpoint) <$> xs)
