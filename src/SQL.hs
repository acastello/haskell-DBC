{-# LANGUAGE GADTs #-}

module SQL where

import Data.ByteString
import qualified Data.IntMap as M
import Data.IntMap (IntMap)
import qualified Data.List as L
import Data.String
import Data.Text.Encoding

import Database.MySQL.Base

import qualified System.IO.Streams as S

import Text.Printf (printf)

class SQL a where
    queryText   :: a -> Query
    fromResult  :: [MySQLValue] -> a

loadSQL :: SQL a => IO [a]
loadSQL = loadSQL' undefined where 
    loadSQL' :: SQL a => a -> IO [a]
    loadSQL' e = do 
        conn <- connect defaultConnectInfo 
            { ciUser = "guest"
            , ciHost = "192.168.1.124"
            , ciDatabase = "world"
            }
        ret <- S.toList . snd =<< query_ conn (queryText e)
        close conn
        return $ fromResult <$> ret

indexSQL :: SQL a => (a -> Int) -> IO (IntMap a)
indexSQL f = M.fromList . fmap (\a -> (f a, a)) <$> loadSQL

simpleSelect :: String -> [String] -> Query
simpleSelect table fields = fromString (printf "SELECT %s FROM %s" comma qtable)
    where
        qtable = printf "`%s`" table :: String
        quotfields = printf "%s.`%s`" qtable <$> fields :: [String]
        comma = L.concat $ L.intersperse "," quotfields

sql_bs :: F ByteString
sql_bs = mkF $ \(MySQLText t) -> encodeUtf8 t

sql_fi :: (Integral a, Num a) => F a
sql_fi = mkF $ \i -> case i of
    MySQLInt8 n -> fromIntegral n
    MySQLInt8U n -> fromIntegral n
    MySQLInt16 n -> fromIntegral n
    MySQLInt16U n -> fromIntegral n
    MySQLInt32 n -> fromIntegral n
    MySQLInt32U n -> fromIntegral n
    MySQLInt64 n -> fromIntegral n
    MySQLInt64U n -> fromIntegral n
    MySQLFloat n -> ceiling n

sql_float :: F Float
sql_float = mkF $ \(MySQLFloat i) -> i

sql_double :: F Double
sql_double = mkF $ \(MySQLDouble i) -> i

-- utils

type F a = Int -> [MySQLValue] -> a
mkF f n xs = f $ xs !! n
