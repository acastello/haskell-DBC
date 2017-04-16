{-# LANGUAGE TemplateHaskell #-}

module Raw_items where
import Data.Serialize
import Codec.Compression.GZip
import Language.Haskell.TH

import Data.IntMap
import Types

serIn :: String -> FilePath -> Q Exp
serIn varname filepath = do
    undefined

g = do
    nm1 <- newName "_"
    nm2 <- newName "_"
    return (LamE [VarP nm1] (LamE [VarP nm2] (VarE nm1)))
    
map' @ Mappings { m_spells = sp } = undefined 
