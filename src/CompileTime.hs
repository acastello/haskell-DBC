{-# LANGUAGE TemplateHaskell #-}

module CompileTime where

import Control.Monad

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

loadN :: Name -> Name -> Int -> Int -> Q Exp
loadN f nam n m = do
    if n <= 1 then
        return $ AppE (AppE (VarE $ mkName ("liftM" ++ show m)) (VarE nam)) (VarE f)
    else do
        exp <- loadN f nam (n-1) m
        return $ AppE exp (VarE f)

liftN :: String -> Int -> Q Exp
liftN f n = do
    nam <- newName "f"
    fs <- loadN (mkName f) nam n n
    return $ ParensE $ LamE [VarP nam] fs
