module Raw_items where
import Data.Serialize
import Codec.Compression.GZip

import Data.IntMap
import Types

raw_items :: IntMap Item
{-# INLINE raw_items #-}