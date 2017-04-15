{-# LANGUAGE DeriveGeneric, DefaultSignatures, GeneralizedNewtypeDeriving #-}

module DBC where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy
import Data.Functor.Identity
import Data.Int
import qualified Data.IntMap as M
import qualified Data.Serialize as S
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Word 

import Foreign

import GHC.Generics

import System.IO (openFile, IOMode (ReadMode), hClose)

import qualified Text.Printf as T

data DBCStatus = DBCStatus
    { dbc_offset    :: Int
    , dbc_position  :: Int64
    } 

data DBCData = DBCData
    { dbc_text  :: ByteString
    , dbc_data  :: ByteString
    }

newtype DBCGet a = DBCGet (ReaderT DBCData (State DBCStatus) a)  
    deriving (Functor, Applicative, Monad, MonadReader DBCData, MonadState DBCStatus)

runGet :: DBCGet a -> DBCData -> DBCStatus -> a
runGet (DBCGet e) da st = evalState (runReaderT e da) st

runDBC :: DBCItem a => DBC ByteString -> DBC a
runDBC dbc = fmap runRow dbc 
    where
        runRow dat = runGet cast (DBCData (strs dbc) dat) (DBCStatus 0 0) 


takeBytes :: Int -> DBCGet [Word8]
takeBytes n = do
    st <- get
    da <- ask
    let newoff = dbc_offset st + (n - 4)
        bytes = BL.take (fi n) $ BL.drop (dbc_position st) (dbc_data da)
    put st
      { dbc_offset = newoff }
    return $ unpack bytes

class DBCItem a where
    cast :: DBCGet a

castCellAt :: DBCItem a => Int -> DBCGet a
castCellAt n = do
    modify $ \s -> s { dbc_position = fromIntegral n * 4 }
    ret <- cast
    modify $ \s -> s { dbc_position = 0 }
    return ret


getW32 :: ByteString -> Word32
getW32 str = L.foldl1 or bytes where
    or = (\a b -> a .|. b `shiftL` 8)
    bytes = fi <$> unpack (BL.take 4 str)

instance DBCItem Word32 where
    cast = do
        xs <- fmap fi <$> takeBytes 4
        return $ L.foldr1 (\a b -> a `shiftL` 8 .|. b) xs

instance DBCItem ByteString where
    cast = do
        DBCData { dbc_text = text } <- ask
        x <- cast :: DBCGet Word32
        return $ BL.drop (fi x) text


-- class Gettable a where
    -- get' :: ByteString -> Get a

    -- default get' :: Serialize a => ByteString -> Get a
    -- get' = \_ -> get

-- instance Gettable ByteString where
    -- get' strs = do
        -- i <- getWord32le
        -- return $ peekBS (BL.drop (fi i) strs)

-- instance Gettable B.ByteString where
    -- get' strs = do
        -- i <- getWord32le
        -- return $ toStrict $ peekBS (BL.drop (fi i) strs)

-- instance Gettable Word8 where
    -- get' = \_ -> getWord8

-- instance Gettable Word16 where
    -- get' = \_ -> getWord16le

-- instance Gettable Word32 where
    -- get' = \_ -> getWord32le

-- instance Gettable Word64 where
    -- get' = \_ -> getWord64le

-- instance Gettable Int8 where
    -- get' = \_ -> getInt8

-- instance Gettable Int16 where
    -- get' = \_ -> getInt16le

-- instance Gettable Int32 where
    -- get' = \_ -> getInt32le

-- instance Gettable Int64 where
    -- get' = \_ -> getInt64le

data DBC a = DBC
    { nrows :: Word32
    , ncols :: Word32
    , rsize :: Word32
    , ssize :: Word32
    , rows  :: M.IntMap a
    , strs  :: ByteString
    } deriving Show

instance Functor DBC where
    fmap = applyf . fmap 

instance Foldable DBC where
    foldMap f = foldMap f . rows

instance Traversable DBC where
    traverse f dbc = (\e -> dbc { rows = e }) <$> traverse f (rows dbc)

readDBC :: FilePath -> IO (DBC ByteString)
readDBC fp = do
    h <- openFile fp ReadMode
    headers <- hGet h 20 
    let (r,c,e,s) = either error id $ flip S.runGetLazy headers $ do
        S.getWord32le
        r <- S.getWord32le
        c <- S.getWord32le
        e <- S.getWord32le
        s <- S.getWord32le
        return (r,c,e,s)
    v <- fmap M.fromList $ replicateM (fi r) $ do
        str <- hGet h (fi e)
        let i = either error id $ S.runGetLazy S.getWord32le str
        return (fi i,str)
    strs <- hGet h (fi s)
    hClose h
    return $ DBC r c e s v strs

-- decodeDBC :: Gettable a => DBC ByteString -> Either String (M.IntMap a)
-- decodeDBC dbc = traverse (runGetLazy $ get' (strs dbc)) (rows dbc)

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

