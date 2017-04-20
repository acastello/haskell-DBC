{-# LANGUAGE TemplateHaskell #-}

module Serialized where

import qualified Data.ByteString.Char8 as B
import Language.Haskell.TH

import Core

serIn :: FilePath -> Q Exp
serIn fp = do
    s <- runIO $ B.readFile fp
    return $ AppE (VarE $ mkName "dec'") (LitE (StringL $ B.unpack s))

loadIn :: Make a => a -> Q Exp
loadIn = serIn . file
