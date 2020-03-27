{-# LANGUAGE OverloadedStrings #-}
module Main where
import CatalogService
import qualified Data.Text as T
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import Control.Monad.Trans.Reader
import Control.Monad
import Database.PostgreSQL.Simple
import Data.Int

main :: IO [Int64]
main = do
    config <- loadMainConfig 
    dbc <- getConnectionInfo config >>= connect
    catalogs <- runReaderT (CatalogService.download "2020-03-18") config
    sequence $ [ case catalog of
        Nothing -> return 0
        Just (date, path) -> CatalogService.parseFromCatalog path >>= CatalogService.addCatalog dbc (date, path) | catalog <- catalogs ]
    
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
