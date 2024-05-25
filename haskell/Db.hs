
{-# LANGUAGE OverloadedStrings  #-}

module Db
    ( Connection, openDb, closeDb
    , runDbTran, DbM
    , Key(..), DbEntity(..)
    , get, getJust, select, selectOrder, add, set, update, delete
    , FromField(..), ToField(..), FromRow(..), ToRow(..), field
    )
where
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

import Data.Text as T
import qualified Data.List as L
import Data.Int
import Data.Semigroup
import System.Directory  -- doesFileExist
import System.IO.Error  -- mkIOError, ...
import Control.Monad.Reader
import Control.Monad.IO.Class   -- imports liftIO

-- ---------------------------------------------------------------
-- Key / DbEntity

newtype Key a = Key { getKey :: Int64 }
    deriving (Eq, Show, Read)

class (FromRow a, ToRow a) => DbEntity a where
    tableName :: proxy a -> Text
    keyName :: proxy a -> Text
    columnNames :: proxy a -> [Text]
    -- Default definitions:
    keyName _ = "id"

instance FromField (Key a) where
    fromField f = Key <$> fromField f

instance ToField (Key a) where
    toField (Key x) = toField x

-- ---------------------------------------------------------------
-- Data base connection

openDb :: Text -> IO Connection
openDb patht = do
    let path = unpack patht
    ok <- doesFileExist path
    if ok then do
        conn <- open path
        execute_ conn "PRAGMA foreign_keys=ON;"
        pure conn
    else
        ioError $ mkIOError doesNotExistErrorType "Cannot open data base file" Nothing (Just path)

closeDb :: Connection -> IO ()
closeDb conn =
    close conn

-- ---------------------------------------------------------------
-- Data base access

type DbM = ReaderT Connection IO

runDbTran :: MonadIO m => Connection -> DbM a -> m a
runDbTran conn dbm = do
    liftIO $ runReaderT dbm conn


tshow :: Text -> Text
tshow name = pack $ show name

interCommas :: [Text] -> Text
interCommas names = T.intercalate "," names

get :: DbEntity a => Key a -> DbM (Maybe a)
get k = ReaderT $ \ conn -> do
    let tabName = tshow $ tableName k
        kName = tshow $ keyName k
        colNames = tshow <$> columnNames k
        q = Query $ "SELECT " <> interCommas colNames <> " FROM " <> tabName <> " WHERE " <> kName <> " = ?"
    rows <- query conn q (Only k)
    case rows of
        [] -> pure Nothing
        [row] -> pure $ Just row

getJust :: DbEntity a => Key a -> DbM a
getJust k =
    get k >>= maybe (fail $ "Invalid foreign key " <> show k) pure

select :: DbEntity a => (Key a -> a -> Bool) -> DbM [(Key a, a)]
select fwhere = ReaderT $ \ conn -> do
    let proxyForFun :: (Key a -> a -> Bool) -> Maybe a
        proxyForFun f = Nothing
        proxy = proxyForFun fwhere
        tabName = tshow $ tableName proxy
        kName = tshow $ keyName proxy
        colNames = tshow <$> columnNames proxy
        q = Query $ "SELECT " <> interCommas (kName : colNames) <> " FROM " <> tabName
        unrow (Only k :. entity) = (k, entity)
    rows <- query_ conn q
    pure $ L.filter (uncurry fwhere) $ unrow <$> rows

selectOrder :: (DbEntity a, Ord fld) => (Key a -> a -> Bool) -> (a -> fld) -> DbM [(Key a, a)]
selectOrder fwhere forderby =
    L.sortOn (forderby . snd) <$> select fwhere

add :: DbEntity a => a -> DbM (Key a)
add value = ReaderT $ \ conn -> do
    let tabName = tshow $ tableName $ Just value
        colNames = tshow <$> (columnNames $ Just value)
        q = Query $ "INSERT INTO " <> tabName <> " (" <> interCommas colNames <> ") VALUES (" <> interCommas (const "?" <$> colNames) <> ")"
    execute conn q value
    Key <$> lastInsertRowId conn

set :: DbEntity a => Key a -> a -> DbM ()
set k value = ReaderT $ \ conn -> do
    let tabName = tshow $ tableName k
        kName = tshow $ keyName k
        colNames = tshow <$> columnNames k
        q = Query $ "UPDATE " <> tabName <> " SET " <> interCommas ((<> "=?") <$> colNames) <> " WHERE " <> kName <> " = ?"
    execute conn q (value :. Only k)

update :: DbEntity a => Key a -> (a -> a) -> DbM ()
update k f = do
    mbvalue <- get k
    case mbvalue of
        Just value -> set k (f value)
        Nothing -> pure ()

delete :: DbEntity a => Key a -> DbM ()
delete k = ReaderT $ \ conn -> do
    let tabName = tshow $ tableName k
        kName = tshow $ keyName k
        colNames = tshow <$> columnNames k
        q = Query $ "DELETE FROM " <> tabName <> " WHERE " <> kName <> " = ?"
    execute conn q (Only k)

