{-# LANGUAGE TemplateHaskell #-}

module Serialized where

import qualified Data.ByteString.Char8 as B
import Language.Haskell.TH

import System.Posix.Files (fileExist)

import Core

serIn :: FilePath -> Q Exp
serIn fp = do
    s <- runIO $ do
        exists <- fileExist fp
        if exists then
            B.readFile fp
        else
            return ""
    return $ AppE (VarE $ mkName "dec'") (LitE (StringL $ B.unpack s))

loadIn :: Make a => a -> Q Exp
loadIn = serIn . file 
