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
    finalResult :: [a] -> M.IntMap a

makeSQL :: SQL a => IO (M.IntMap a)
makeSQL = finalResult <$> loadSQL

loadSQL :: SQL a => IO [a]
loadSQL = loadSQL' undefined where
    loadSQL' :: SQL a => a -> IO [a]
    loadSQL' e = do
        conn <- connect defaultConnectInfo
            { ciUser = "guest"
            , ciHost = "localhost"
            , ciDatabase = "world"
            }
        ret <- S.toList . snd =<< query_ conn (queryText e)
        close conn
        return $ fromResult <$> ret

indexSQL :: SQL a => (a -> Int) -> IO (IntMap a)
indexSQL f = M.fromList . fmap (\a -> (f a, a)) <$> loadSQL

mergeSQL :: SQL a => (a -> Int) -> IO (IntMap [a])
mergeSQL f = M.fromListWith (++) . fmap (\a -> (f a, [a])) <$> loadSQL

simpleSelect :: String -> [String] -> Query
simpleSelect table fields = fromString (printf "SELECT %s FROM %s" comma qtable)
    where
        qtable = printf "`%s`" table :: String
        quotfields = printf "%s.`%s`" qtable <$> fields :: [String]
        comma = L.concat $ L.intersperse "," quotfields

sql_bs :: F ByteString
sql_bs = mkF $ \i -> case i of
    MySQLText t -> encodeUtf8 t
    MySQLNull -> ""

sql_fi :: Num a => F a
sql_fi = mkF $ \i -> case i of
    MySQLInt8 n -> fromIntegral n
    MySQLInt8U n -> fromIntegral n
    MySQLInt16 n -> fromIntegral n
    MySQLInt16U n -> fromIntegral n
    MySQLInt32 n -> fromIntegral n
    MySQLInt32U n -> fromIntegral n
    MySQLInt64 n -> fromIntegral n
    MySQLInt64U n -> fromIntegral n

sql_float :: F Float
sql_float = mkF $ \i -> case i of
    MySQLFloat n -> n
    MySQLDouble n -> realToFrac n

sql_double :: F Double
sql_double = mkF $ \i -> case i of
    MySQLFloat n -> realToFrac n
    MySQLDouble n -> n

-- utils

q :: Query -> IO [[MySQLValue]]
q tab = do
    conn <- connect defaultConnectInfo { ciUser = "guest"
        , ciHost = "192.168.1.124", ciDatabase = "world" }
    ret <- S.toList . snd =<< query_ conn tab
    close conn
    return ret

type F a = Int -> [MySQLValue] -> a
mkF f n xs = f $ xs !! n
