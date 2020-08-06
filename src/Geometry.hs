module Geometry where

import Data.List
import Data.Maybe
import qualified Data.List as L

import Core

type Polygon = [Point]

contained :: Point -> Polygon -> Bool
contained _ [] = False
contained p (head:pol) = length left `mod` 2 /= 0 && length right `mod` 2 /= 0
  where
    (left, right) = partition (< p_x p) crosses
    crosses = fmap (uncurry hei) edges
    edges = catMaybes $ snd $ mapAccumR f head pol
    f p' p'' =
        if signum (p_y p - p_y p') /= signum (p_y p - p_y p'') then
            (p'', Just (p',p''))
        else
            (p'', Nothing)
    hei :: Point -> Point -> Float
    hei p' p'' = m * (p_y p) + n where
        m = (p_x p'' - p_x p') / (p_y p'' - p_y p')
        n = p_x p' - m * p_y p'
