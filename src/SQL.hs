module SQL where

import Database.MySQL.Base

import qualified System.IO.Streams as S

class SQL a where
    queryText   :: a -> Query
    fromResult  :: [MySQLValue] -> a

loadSQL :: SQL a => IO [a]
loadSQL = do 
