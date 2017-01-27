{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module DBC where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy
import Data.Int
import Data.Serialize
import Data.Vector
import qualified Data.Vector as V
import Data.Word 

import GHC.Generics

import System.IO (openFile, IOMode (ReadMode), hClose)

import qualified Text.Printf as T

class Gettable a where
    get' :: ByteString -> Get a

    default get' :: Serialize a => ByteString -> Get a
    get' = \_ -> get

instance Gettable ByteString where
    get' strs = do
        i <- getWord32le
        return $ peekBS (BL.drop (fi i) strs)

instance Gettable B.ByteString where
    get' strs = do
        i <- getWord32le
        return $ toStrict $ peekBS (BL.drop (fi i) strs)

instance Gettable Word8 where
    get' = \_ -> getWord8

instance Gettable Word16 where
    get' = \_ -> getWord16le

instance Gettable Word32 where
    get' = \_ -> getWord32le

instance Gettable Word64 where
    get' = \_ -> getWord64le

instance Gettable Int8 where
    get' = \_ -> getInt8

instance Gettable Int16 where
    get' = \_ -> getInt16le

instance Gettable Int32 where
    get' = \_ -> getInt32le

instance Gettable Int64 where
    get' = \_ -> getInt64le

data DBC a = DBC
    { nrows :: Word32
    , ncols :: Word32
    , rsize :: Word32
    , ssize :: Word32
    , rows  :: Vector a
    , strs  :: ByteString
    } deriving Show

instance Functor DBC where
    fmap f = applyf (fmap f)

instance Foldable DBC where
    foldMap f = foldMap f . rows

instance Traversable DBC where
    traverse f dbc = (\e -> dbc { rows = e }) <$> traverse f (rows dbc)

readDBC :: FilePath -> IO (DBC ByteString)
readDBC fp = do
    h <- openFile fp ReadMode
    headers <- hGet h 20 
    let (r,c,e,s) = either error id $ flip runGetLazy headers $ do
        get :: Get Word32
        r <- getWord32le
        c <- getWord32le
        e <- getWord32le
        s <- getWord32le
        return (r,c,e,s)
    v <- generateM (fi r) (\_ -> hGet h (fi e))
    strs <- hGet h (fi s)
    hClose h
    return $ DBC r c e s v strs

decodeDBC :: Gettable a => DBC ByteString -> Either String (Vector a)
decodeDBC dbc = traverse (runGetLazy $ get' (strs dbc)) (rows dbc)

--
-- utils
--

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral 

peekBS :: ByteString -> ByteString
peekBS = BL.takeWhile (/=0)

applyf f dbc = dbc { rows = f (rows dbc) }

isInfix :: B.ByteString -> B.ByteString -> Bool
isInfix needle hay = Prelude.any (B.isPrefixOf needle) 
                     [B.drop i hay | i <- [0.. B.length hay - B.length needle]]
knownspells :: [Word32]
knownspells = [18053          -- sp
              , 23727, 15464  -- hit
              , 7598, 7597    -- crit
              , 9331, 14027   -- ap
              , 21362, 21626  -- mp5
              , 25975         -- spen
              , 21598         -- hp5
              , 13390         -- def
              , 13665         -- par
              , 13669         -- ddg
              ]

