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

main :: IO [[Comic]]
main = do
    config <- loadMainConfig 
    files <- runReaderT (download "2020-03-18") config
    sequence [ case file of 
        Nothing -> return [] 
        Just f -> parseFromCatalog f | file <- files]

loadMainConfig :: IO DC_T.Config
loadMainConfig = DC.load $ [DC.Required "application.properties"] 

parseAFile = parseFile "/Users/slemoine/dev/workspace/comicspreviews-parser/march.txt"

getConnectionInfo :: DC_T.Config -> IO ConnectInfo
getConnectionInfo config = do 
    host <- DC.require config . T.pack $ "db_host"
    port <- DC.require config . T.pack $ "db_port"
    username <- DC.require config . T.pack $ "db_username"
    password <- DC.require config . T.pack $ "db_password"
    database <- DC.require config . T.pack $ "db_database"
    return $ ConnectInfo host port username password database

connectToDatabase catalog = 
    let insert = \c -> execute c "INSERT INTO comicspreviews.t_catalog (date_creation,filepath) VALUES (?,?)" catalog
    in loadMainConfig >>= getConnectionInfo >>= connect >>= insert

