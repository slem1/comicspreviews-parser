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
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import qualified Control.Logging as LOGGER

data PArgs = PArgs {
    date :: String
    ,correlationId :: String
    ,overwrite :: Bool
} deriving (Show)

pargs :: Parser PArgs 
pargs = PArgs <$> strOption 
                (   long "date"
                    <> short 'd'
                    <> metavar "DATE"
                    <> help "reference date for import"
                )
            <*> strOption
                (   long "correlationId"
                    <> short 'i'
                    <> metavar "CORRELATION_ID"
                    <> help "Optional correlation id"
                )
            <*> switch 
                (
                    long "overwrite"
                    <> short 'o'                
                    <> help "overwrite previous import"
                )    

pargsInfo :: ParserInfo PArgs
pargsInfo = info (pargs <**> helper) ( fullDesc <> progDesc "Import comics catalog")


main :: IO [Int64]
main = do
    args <- execParser pargsInfo
    let referenceDate = (parseTimeOrError True defaultTimeLocale "%Y-%m-%d" $ date args) :: Day    
    config <- loadMainConfig 
    logFile <- DC.require config . T.pack $ "log_file"
    LOGGER.withFileLogging logFile $ do
        LOGGER.log (T.pack $ "program started with arguments: " ++ show args)
        dbc <- getConnectionInfo config >>= connect
        catalogs <- runReaderT (CatalogService.download referenceDate) config        
        sequence $ [ case catalog of
            Nothing -> LOGGER.log (T.pack "Nothing downloaded") >> return (-1)
            Just c -> parseAndInserts dbc c | catalog <- catalogs ]  
    where        
        parseAndInserts conn (date,path) = do 
            LOGGER.log (catLog (date,path)) 
            comics <- CatalogService.parseFromCatalog path 
            LOGGER.log $ T.pack (show (length comics) ++ " comics parsed") 
            result <- CatalogService.addCatalog conn (date, path) comics  
            LOGGER.log $ T.pack "injection done"
            return result
        catLog (date, path) = T.pack ("catalog from " ++ show date ++ " downloaded in " ++ path)    
    
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
