module Geomtry where

import Types

type Polygon = [Point]

contained :: Point -> Polygon -> Bool
contained p pol
