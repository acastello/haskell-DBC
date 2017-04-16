{-# LANGUAGE DeriveGeneric, DefaultSignatures, GeneralizedNewtypeDeriving #-}

module DBC where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString
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
    , dbc_position  :: Int
    } 

data DBCData = DBCData
    { dbc_text  :: ByteString
    , dbc_data  :: ByteString
    }

newtype DBCGet a = DBCGet (ReaderT DBCData (State DBCStatus) a)  
    deriving (Functor, Applicative, Monad, MonadReader DBCData, MonadState DBCStatus)

runGet :: DBCGet a -> DBCData -> DBCStatus -> a
runGet (DBCGet e) da st = evalState (runReaderT e da) st

castDBC :: DBCItem a => DBC ByteString -> DBC a
castDBC dbc = fmap runRow dbc 
    where
        runRow dat = runGet cast (DBCData (strs dbc) dat) (DBCStatus 0 0) 

indexDBC :: DBCItem a => (a -> Int) -> DBC a -> M.IntMap a
indexDBC f dbc = M.fromList $ (\a -> (f a, a)) <$> rows dbc

loadIndexed :: DBCItem a => FilePath -> (a -> Int) -> IO (M.IntMap a)
loadIndexed fp f = indexDBC f . castDBC <$> loadDBC fp

takeBytes :: Int -> DBCGet [Word8]
takeBytes n = do
    st <- get
    da <- ask
    let newoff = dbc_offset st + (n - 4)
        bytes = B.take (fi n) $ B.drop (dbc_position st) (dbc_data da)
    -- put st { dbc_offset = newoff }
    return $ unpack bytes

class DBCItem a where
    cast :: DBCGet a

castAt :: DBCItem a => Int -> DBCGet a
castAt n = do
    modify $ \s -> s { dbc_position = fromIntegral n * 4 }
    ret <- cast
    modify $ \s -> s { dbc_position = 0 }
    return ret

castW32At :: Int -> DBCGet Word32
castW32At = castAt

getW32 :: ByteString -> Word32
getW32 str = L.foldl1 or bytes where
    or = (\a b -> a `shiftL` 8 .|. b)
    bytes = fi <$> unpack (B.take 4 str)

castWord32 :: DBCGet Word32
castWord32 = do
    xs <- fmap fi <$> takeBytes 4
    return $ L.foldr1 (\a b -> a .|. b `shiftL` 8) xs

instance DBCItem Word32 where
    cast = castWord32

castInt32 :: DBCGet Int32
castInt32 = do
    xs <- fmap fi <$> takeBytes 4
    return $ L.foldr1 (\a b -> a .|. b `shiftL` 8) xs

instance DBCItem Int where
    cast = fi <$> castWord32

instance DBCItem Int32 where
    cast = castInt32

instance DBCItem BL.ByteString where
    cast = BL.fromStrict <$> cast

instance DBCItem ByteString where
    cast = do
        DBCData { dbc_text = text } <- ask
        x <- cast :: DBCGet Word32
        return $ peekBS $ B.drop (fi x) text

data DBC a = DBC
    { nrows :: Word32
    , ncols :: Word32
    , rsize :: Word32
    , ssize :: Word32
    , rows  :: [a]
    , strs  :: ByteString
    } deriving Show

instance Functor DBC where
    fmap = applyf . fmap 

instance Foldable DBC where
    foldMap f = foldMap f . rows

instance Traversable DBC where
    traverse f dbc = (\e -> dbc { rows = e }) <$> traverse f (rows dbc)

loadDBC :: FilePath -> IO (DBC ByteString)
loadDBC fp = do
    (headers, str) <- B.splitAt 20 <$> B.readFile fp
    let (r,c,e,s) = either error id $ flip S.runGet headers $ do
        S.getWord32le
        r <- S.getWord32le
        c <- S.getWord32le
        e <- S.getWord32le
        s <- S.getWord32le
        return (r,c,e,s)
    let f str n = 
          if n <= 0 then 
              ([], str) 
          else
              let (left, right) = B.splitAt (fi e) str
                  (result, remaining) = f right (n-1)
              in (left:result, remaining)
        (rows, text) = f str r
    seq (headers,str) $ return $ DBC r c e s rows text

-- decodeDBC :: Gettable a => DBC ByteString -> Either String (M.IntMap a)
-- decodeDBC dbc = traverse (runGetLazy $ get' (strs dbc)) (rows dbc)

--
-- utils
--

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral 

peekBS :: ByteString -> ByteString
peekBS = B.takeWhile (/=0)

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

