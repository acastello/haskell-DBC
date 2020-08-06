{-# LANGUAGE
    MultiParamTypeClasses
  , ExistentialQuantification
  , TypeFamilies
  , FlexibleInstances
  , TemplateHaskell
  , RankNTypes
  , TypeSynonymInstances #-}

module Query
  ( module Query
  , module Core
  , module Source
  , module OpenGL
  , module Geometry
  , M.size, M.lookup
  , printf
  ) where

import Control.Concurrent
import Control.Monad

import qualified Data.ByteString.Char8 as B
import Data.Function
import qualified Data.IntMap as M
import qualified Data.Map as Ma
import Data.Maybe
import qualified Data.List as L
import Data.String
import Data.Word

import System.IO.Unsafe
import System.Posix.ByteString (fileExist)
import System.Process (callCommand)

import Text.Printf

import Core
import Source
import Geometry
import OpenGL

export filepath = save filepath . fmap (p2t . pos) . toList'

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

by_ :: (a -> b) -> C a b c
by_ = flip (.)

in_ xs = \x -> any (== x) xs

filters :: Filterable f => [a -> Bool] -> f a -> f a
filters con = filter' (\e -> L.all ($ e) con)

sorts :: (Listable f, Ord c) => C a c c -> f a -> [a]
sorts f = L.reverse . L.sortOn (f id) . toList'

sorts' :: (Filterable f, Listable f, Ord b, Num b)
                      => (forall c. C a b c) -> f a -> [a]
sorts' f = L.reverse . L.sortOn (f id) . toList' . filters [f (/=0)]

takes = L.take

groups :: (Listable f, Eq c) => C a c c -> f a -> [(c,[a])]
groups f = fmap (\xs -> (f id (head xs), xs)) .
           L.groupBy (\a b -> f id a == f id b) . toList'

groups' :: (Listable f, Eq c, Ord c) => C a c c -> f a -> [(c, [a])]
groups' f = groups f . sorts f

groups'' :: (Filterable f, Listable f, Ord b, Num b)
                      => (forall c. C a b c) -> f a -> [(b, [a])]
groups'' f = groups f . sorts' f

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
by_it_id = by_ it_id
by_it_name = by_ it_name
by_it_slot = by_ it_slot
by_it_mat = by_ it_mat
by_it_stats = by_ it_stats
by_it_level = by_ it_level
by_it_qual = by_ it_qual
by_it_rlevel = by_ it_rlevel
by_it_desc = by_ it_desc
by_it_score tab = by_ (\i -> score tab (it_stats i)
               (su_stats <$> L.filter (\s -> su_chance s > 2) (it_suffs i)))

instance IsString Item where
    fromString str = last $ toList' $ filters [by_it_name (== B.pack str)] items

-- suffix getters
-- by_se_id = by_ se_id
-- by_se_val = by_ se_val
-- by_se_type = by_ se_type
-- by_se_desc = by_ se_desc

-- GameObject getters
by_go_id      = by_ go_id
by_go_name    = by_ go_name
by_go_p       = by_ go_point
by_go_m       = by_ (p_m . go_point)
by_go_cd      = by_ go_cd
by_go_dist    = \p -> by_ (dist p . go_point)

-- Spell getters
by_sp_id      = by_ sp_id
by_sp_reag    = by_ sp_reag
by_sp_prod    = by_ sp_prod
by_sp_disen   = by_ sp_disen

instance IsString Spell where
    fromString str = last $ toList' $ filters [by_ sp_name (== B.pack str)] spells

sp_pratio :: Spell -> Double
sp_pratio s = pprod / preag where
    preag = sum $ fmap (\(id, n) ->
            fi n * (maybe 0 (fi . it_price) (M.lookup id items))) (sp_reag s)
    pprod = sum $ fmap (\(id, n) ->
            n * (maybe 0 (fi . it_price) (M.lookup id items))) (sp_prod s)

sp_pratio' :: [(ItemId, Double)] -> Spell -> Double
sp_pratio' ws s = pprod / wei where
    pprod = sum $ fmap (\(id, n) ->
            n * (maybe 0 (fi . it_price) (M.lookup id items))) (sp_prod s)
    wei = weight ws (sp_reag s)

sp_disen s = do
        (iid, quant) <- sp_prod s
        maybe [] id $ do
            it <- M.lookup iid items
            return $ fmap (fmap (*quant)) (it_disen it)


has_reagent rs = by_sp_reag (any ((\e -> any (== e) rs) . fst))

has_product ps = by_sp_prod (any ((\e -> any (== e) ps) . fst))

exists path = fileExist $ mconcat ["Icons/", path, ".png"]

-- weight :: [a -> Double] -> a -> Double
-- weight xs a = sum $ ($ a) <$> xs

weight :: (Eq a, Real b, Real c) => [(a, b)] -> [(a, c)] -> Double
weight xs ys = sum $ do
    (x, wei) <- xs
    (y, amo) <- ys
    if x == y then
        return (any2d wei * any2d amo)
    else
        return 0

eff :: (a -> Double) -> (a -> Double) -> a -> Double
eff = liftM2 (/)

-- efficiency xs ys e =

disenchantEff ew iw = liftM2 (/) (by_sp_disen (weight ew)) (by_sp_reag (weight iw))

-- shorter distances between two lists

-- shortestPath :: (Listable f, Listable f1, Position a, Position b)
  -- => f a -> f1 b -> [(Float, a, b)]
shortestPath xs ys = fmap (L.minimumBy (compare `on` fst') . snd)
                      $ groups' (by_ snd') $ do
    x <- toList' xs
    y <- toList' ys
    return (dist (pos x) (pos y), x, y) where
        fst' (x,_,_) = x
        snd' (_,x,_) = x


-- generic comparing functions
is_instance n = not $ any (== n) [0, 1, 530, 571]

is_herb id = any (== id) herbIds

herbIds =
  [ 1617,   1618,   1629,   1620,   1621,   1622,   1623,   1624,   1625,   1628
  , 2041,   2042,   2043,   2044,   2045,   2046,   2866,   3724,   3726
  , 3727,   3729,   3730,   3725,   142140, 142141, 142142, 142143, 142144
  , 142145, 176583, 176584, 176586, 176587, 176588, 176589, 176636, 176637
  , 176638, 176639, 176640, 176641, 176642, 180164, 180165, 180166, 180167
  , 180168, 181166, 181270, 181271, 181275, 181277, 181278, 181279
  , 181280, 181281, 183043, 183044, 183045, 183046, 185881, 189973, 190169
  , 190170, 190171, 190172, 190173, 190174, 190175, 190176, 191019, 191303 ]

herbs :: M.IntMap GameObject
herbs = filters [by_go_id is_herb] gameObjects

is_vein id = any (== id)
  [ 181556, 185557, 189978, 1731, 3763, 2055, 181248, 103713, 165658, 181555
  , 1734, 150080, 181109, 180215, 1610, 1667, 19903, 1735, 181557, 181069
  , 2653, 2040, 150079, 176645, 185877, 73941, 123310, 177388, 73940, 123848
  , 123309, 181569, 181570, 189979, 189981, 175404, 189980, 1733, 105569
  , 181068, 324, 150082, 176643, 1732, 3764, 2054, 181249, 103711, 191133
  , 2047, 181108, 150081]

veins :: M.IntMap GameObject
veins = filters [by_go_id is_vein] gameObjects

score :: [(Stat,Double)] -> [(Stat,Int)] -> [[(Stat,Int)]] -> Double
score tab hay [] = L.sum $ do
    (s,v) <- tab
    (s', v') <- hay
    if s == s' then
        return (v * fromIntegral v')
    else
        []
score tab hay opt = score tab hay [] + maximum ((\l -> score tab l []) <$> opt)

-- std :: Int -> [(Stat, Double)] -> [Item -> Bool] -> [Item]
-- std n scoretab filts = takes n $ sorts (by_it_score scoretab) $ filters filts raw_items

-- std' xs = loadGameObjects >>= \gos -> foldMap print $ filters xs gos

dmg = [(Damage, 1.0)]

by_speed :: C Item Double Double
by_speed = by_ $ \i -> L.sum $ do
    (s,n) <- it_stats i
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

hunAgiScore = [ (Agility, 1.5), (Hit, 0.75), (Intellect, 0.7), (AttackPower, 0.5)
              , (Crit, 0.5), (Expertise, 1), (Strength, 0.5) ]

hunArpScore = [ (ArmorPen, 1.5), (Agility, 1.0), (Hit, 0.75), (Intellect, 0.7), (AttackPower, 0.5)
              , (Crit, 0.5), (Expertise, 1), (Strength, 0.5) ]

rogueScore = [ (Agility, 1), (Hit, 1.5), (AttackPower, 0.6), (Crit, 0.4)
             , (Expertise, 1.5), (Strength, 0.6) ]

warlockScore = [ (SpellPower, 1), (Hit, 2), (Intellect, 0.2), (Crit, 0.3)
               , (Haste, 0.5), (SpellPen, 1) ]

intScore = [ (SpellPower, 1), (Hit, 2), (Intellect, 0.75), (Crit, 0.5)
           , (Haste, 0.5), (SpellPen, 1) ]

spScore = [ (SpellPower, 1), (Hit, 0.5), (Intellect, 0.5), (Crit, 0.5)
          , (Haste, 0.5), (SpellPen, 1) ]

spCrit = [ (SpellPower, 1), (Hit, 0.5), (Intellect, 0.25), (Crit, 0.85)
         , (Haste, 0.40), (Spirit, 0.5)]

spHaste = [ (SpellPower, 1), (Hit, 0.5), (Intellect, 0.15), (Crit, 0.35)
         , (Haste, 0.95), (Spirit, 0.5)]

mp5Score = [ (Spirit, 0.5), (ManaPer5, 1), (Intellect, 0.2) ]

healScore = mp5Score ++ spScore

iurl id = callCommand $ "xdg-open http://wotlk.evowow.com/?item=" ++ show id

-- util

any2d :: (Real a, Fractional c) => a -> c
any2d = fromRational . toRational

showItId id = maybe "" show (M.lookup id items)

i :: B.ByteString -> ItemId
i name = it_id $ head $ toList' $ filters [by_it_name (== name)] items

s :: B.ByteString -> SpellId
s name = sp_id $ head $ toList' $ filters [by_ sp_name (== name)] spells

instance Bidimensional Point where
    vertp p = pure $ (,) (- (realToFrac $ p_y p)) (realToFrac $ p_x p)
    vert Point { p_x = x, p_y = y } = vertex $ Vertex2 (-y) x
    bounds Point { p_x = x, p_y = y }
                         = bounds (realToFrac (-y) :: Double, realToFrac x :: Double)

instance Bidimensional GameObject where
    vertp = vertp . go_point
    vert = vert . go_point
    bounds = bounds . go_point

p2tuple :: Point -> (Float, Float, Float, Word32)
p2tuple p = (p_x p, p_y p, p_z p, fromIntegral $ p_m p)

-- saveGO :: M.IntMap GameObject -> IO ()
-- saveGO [] = error "no gameobjects"
-- saveGO xs = save ("tup4_" ++ B.unpack (go_name $ head xs) ++ ".gz")
--                   ((p2tuple . go_point) <$> xs)
