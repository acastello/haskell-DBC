{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

module DBC where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy
import Data.Int
import Data.Serialize
import Data.Vector
import qualified Data.Vector as V
import Data.Word 

import GHC.Generics

import System.IO (openFile, IOMode (ReadMode), hClose)

class Gettable a where
    get' :: ByteString -> Get a

    default get' :: Serialize a => ByteString -> Get a
    get' = \_ -> get

instance Gettable ByteString where
    get' strs = do
        i <- getWord32le
        return $ peekBS (BL.drop (fi i) strs)

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

isInfix :: ByteString -> ByteString -> Bool
isInfix needle hay = Prelude.any (isPrefixOf needle) 
                     [BL.drop i hay | i <- [0.. BL.length hay - BL.length needle]]

data Spell = Spell
    { sp_id   :: Word32     -- 0
    , sp_n    :: Int32     -- 80
    , sp_type :: Word32     -- 110
    , sp_desc :: ByteString -- 136 
    } deriving Generic

instance Show Spell where
    show (Spell id n typ desc) = show (id, n, typ, desc) Prelude.++ "\n"

instance Gettable Spell where
    get' strs = do
        id <- get' strs
        skip (4*79)
        n <- get' strs
        skip (4*29)
        typ <- get' strs
        skip (4*25)
        desc <- get' strs
        return $ Spell id desc n typ

newtype T136 a = T136 a
    deriving (Generic)

instance Show a => Show (T136 a) where
    show (T136 x) = show x

instance Serialize a => Serialize (T136 a) where

instance Gettable a => Gettable (T136 a) where
    get' strs = do
        skip 544
        T136 <$> get' strs
