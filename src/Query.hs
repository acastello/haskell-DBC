{-# LANGUAGE MultiParamTypeClasses #-}

module Query where

import Source
import Types

class Ord b => Element a b where
    elof      :: a -> b
    findElem  :: a -> b
    
instance Element Item Slot 
