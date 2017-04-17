{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Serialized where
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Serialize
import Codec.Compression.GZip
import Language.Haskell.TH

import Data.IntMap
-- import Types
-- import Source

serIn :: FilePath -> Q Exp
serIn fp = do
    s <- runIO $ B.readFile fp
    return $ AppE (VarE $ mkName "dec'") (LitE (StringL $ B.unpack s))

