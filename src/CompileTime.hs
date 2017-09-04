{-# LANGUAGE 
        TemplateHaskell,        CPP
      , MagicHash,              TypeOperators
      , TypeSynonymInstances,   FlexibleInstances
      , FlexibleContexts                                  #-}

module CompileTime where

import Control.Monad

import qualified Data.ByteString.Char8 as B

-- #if MIN_VERSION_template_haskell(2,8,0)
import Data.Char (ord)
import qualified Data.IntMap as M
import Data.Word (Word8)
-- #endif

import Generics.Deriving

import GHC.Base (unpackCString#)
import GHC.Exts

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

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

serIn' x = do
    let fp = file $ M.fromList [(0, x)]
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

genericLift :: (Generic a, GLift (Rep a)) => a -> Q Exp
genericLift = glift "" . from

class GLift f where
    glift :: String -- ^ The package name (not used on GHC 8.0 and later)
          -> f a    -- ^ The generic value
          -> Q Exp  -- ^ The resulting Template Haskell expression

instance (Datatype d, GLiftDatatype f) => GLift (D1 d f) where
    glift _pkg d@(M1 x) = gliftWith pName mName x
      where
        pName, mName :: String
#if __GLASGOW_HASKELL__ >= 711
        pName = packageName d
#else
        pName = _pkg
#endif
        mName = moduleName d:: String

class GLiftDatatype f where
    gliftWith :: String -- ^ The package name
              -> String -- ^ The module name
              -> f a    -- ^ The generic value
              -> Q Exp  -- ^ The resulting Template Haskell expression

instance (Constructor c, GLiftArgs f) => GLiftDatatype (C1 c f) where
    gliftWith pName mName c@(M1 x) =
      appsE (conE (mkNameG_d pName mName cName) : gliftArgs x)
      where
        cName :: String
        cName = conName c

instance (GLiftDatatype f, GLiftDatatype g) => GLiftDatatype (f :+: g) where
    gliftWith pName mName (L1 l) = gliftWith pName mName l
    gliftWith pName mName (R1 r) = gliftWith pName mName r

class GLiftArgs f where
    gliftArgs :: f a -> [Q Exp]

instance GLiftArgs U1 where
    gliftArgs U1 = []

instance Lift c => GLiftArgs (K1 i c) where
    gliftArgs (K1 x) = [lift x]

instance GLiftArgs f => GLiftArgs (S1 s f) where
    gliftArgs (M1 x) = gliftArgs x

instance (GLiftArgs f, GLiftArgs g) => GLiftArgs (f :*: g) where
    gliftArgs (f :*: g) = gliftArgs f ++ gliftArgs g

instance GLiftArgs UAddr where
    gliftArgs (UAddr a) = [litE (stringPrimL (word8ify (unpackCString# a)))]
      where
-- #if MIN_VERSION_template_haskell(2,8,0)
        word8ify :: String -> [Word8]
        word8ify = map (fromIntegral . ord)
-- #else
        -- word8ify :: String -> String
        -- word8ify = id
-- #endif

-- #if MIN_VERSION_template_haskell(2,11,0)
instance GLiftArgs UChar where
    gliftArgs (UChar c) = [litE (charPrimL (C# c))]
-- #endif

instance GLiftArgs UDouble where
    gliftArgs (UDouble d) = [litE (doublePrimL (toRational (D# d)))]

instance GLiftArgs UFloat where
    gliftArgs (UFloat f) = [litE (floatPrimL (toRational (F# f)))]

instance GLiftArgs UInt where
    gliftArgs (UInt i) = [litE (intPrimL (toInteger (I# i)))]

instance GLiftArgs UWord where
    gliftArgs (UWord w) = [litE (wordPrimL (toInteger (W# w)))]

instance Lift B.ByteString where
    lift = return . LitE . StringL . B.unpack 
instance Lift Point where
    lift = genericLift
