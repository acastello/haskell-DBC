module DBC where

import Data.ByteString.Lazy
import Data.Int
import Data.Serialize
import Data.Vector
import Data.Word 

import System.IO (openFile, IOMode (ReadMode), hClose)

data DBC a = DBC
    { rows  :: Word32
    , cols  :: Word32
    , esize :: Word32
    , ssize :: Word32
    , ents  :: Vector a
    } deriving Show

instance Functor DBC where
    fmap f dbc = dbc { ents = fmap f (ents dbc) }

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
    hClose h
    return $ DBC r c e s v


--
-- utils
--

fi = fromIntegral 
