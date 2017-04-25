{-# LANGUAGE TemplateHaskell #-}

module CompileTime where

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

