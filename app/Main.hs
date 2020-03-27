{-# LANGUAGE OverloadedStrings #-}
module Main where
import Lib
import Parser
import qualified Data.Text as T
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import Control.Monad.Trans.Reader
import Comic
import Database.PostgreSQL.Simple
import Control.Monad
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Int

main :: IO [Int64]
main = do
    config <- loadMainConfig 
    dbc <- getConnectionInfo config >>= connect
    catalogs <- runReaderT (download "2020-03-18") config
    sequence $ [ case catalog of
        Nothing -> return 0
        Just (date, path) -> parseFromCatalog path >>= insertCatalog dbc (date, path) | catalog <- catalogs ]
    
loadMainConfig :: IO DC_T.Config
loadMainConfig = DC.load $ [DC.Required "application.properties"] 

getConnectionInfo :: DC_T.Config -> IO ConnectInfo
getConnectionInfo config = do 
    host <- DC.require config . T.pack $ "db_host"
    port <- DC.require config . T.pack $ "db_port"
    username <- DC.require config . T.pack $ "db_username"
    password <- DC.require config . T.pack $ "db_password"
    database <- DC.require config . T.pack $ "db_database"
    return $ ConnectInfo host port username password database

insertCatalog :: Connection -> (Day, FilePath) -> [Comic] -> IO Int64
insertCatalog conn (date, path) comics = 
    let dateStr = formatTime defaultTimeLocale "%Y-%m-%d" date
    in withTransaction conn $ insertCatalogLine conn (date, path) >>= \(Only idCatalog)-> insertComicsLines conn comics idCatalog

insertCatalogLine :: Connection -> (Day, FilePath) -> IO (Only Int)
insertCatalogLine conn catalog = do
    [x] <- query conn "INSERT INTO comicspreviews.t_catalog (date_creation,filepath) VALUES (?,?) RETURNING id_t_catalog" catalog
    return x

insertComicsLines :: Connection -> [Comic] -> Int -> IO Int64
insertComicsLines conn comics idCatalog = executeMany conn "INSERT INTO comicspreviews.t_comic (id_t_catalog, reference, title, price, editor) VALUES (?, ?, ?, ?, ?)" (mapper <$> comics)
    where
        mapper (Comic id title price editor) = (idCatalog, id, title, price, editor)


