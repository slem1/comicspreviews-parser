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
import qualified Util as U
import Control.Monad.Trans.Class
import Crypto

data PArgs = PArgs {
    date :: String
    ,offset :: Integer
    ,correlationId :: String    
} deriving (Show)

pargs :: Parser PArgs 
pargs = PArgs <$> strOption 
                (   long "date"
                    <> short 'd'
                    <> metavar "DATE"
                    <> help "reference date for import"
                )
            <*> option auto 
                (   long "off"
                    <> metavar "OFFSET"
                    <> help "The week offset"                
                ) 
            <*> strOption
                (   long "correlationId"
                    <> short 'i'
                    <> metavar "CORRELATION_ID"
                    <> help "Optional correlation id"
                )            

pargsInfo :: ParserInfo PArgs
pargsInfo = info (pargs <**> helper) ( fullDesc <> progDesc "Import comics catalog")


main :: IO [Int64]
main = do
    config <- loadMainConfig 
    logFile <- DC.require config . T.pack $ "log_file"
    LOGGER.withFileLogging logFile $ do
        mrds <- init
        case mrds of 
            Nothing -> LOGGER.log (T.pack "An error occured during initialization") >> return []
            Just rds -> runReaderT (start rds) config 
        where
            init = do        
                args <- execParser pargsInfo 
                LOGGER.log (T.pack $ "program started with arguments: " ++ show args)                
                date <- parseTimeM True defaultTimeLocale "%Y-%m-%d" $ date args
                return $ releaseDays date (offset args)    


start :: [Day] -> ReaderT DC_T.Config IO [Int64]
start releaseDays = ask >>= (\config -> lift $ do 
        dbc <- getConnectionInfo config >>= connect
        sequence $ [ do 
            catalog <- CatalogService.isCatalogExist dbc rd >>= downloadCatalog config rd
            case catalog of
                Just c -> parseCatalogAndInject dbc c
                Nothing -> return (-1)                
         | rd <- releaseDays ]        
        ) 
    where
        downloadCatalog :: DC_T.Config -> Day -> Bool -> IO (Maybe (Day, FilePath))
        downloadCatalog config rd isCatalogExists = 
            if isCatalogExists                
                then LOGGER.log (T.pack ("catalog from " ++ (show rd) ++ " already exists")) >> return Nothing                
                else do                    
                    url <- DC.require config (T.pack "previewsworld_url")
                    outputDir <- DC.require config (T.pack "output_dir")     
                    CatalogService.downloadReleases url rd outputDir
                    

parseCatalogAndInject :: Connection -> (Day, FilePath) -> IO Int64
parseCatalogAndInject conn (date,path) = do 
    LOGGER.log (catLog (date,path)) 
    parseResult <- CatalogService.parseFromCatalog path date
    case parseResult of                
        Left err -> (LOGGER.log $ T.pack err) >> fail err          
        Right comics -> do                      
            LOGGER.log $ T.pack (show (length comics) ++ " comics parsed") 
            result <- CatalogService.addCatalog conn (date, path) comics  
            LOGGER.log $ T.pack "injection done"
            return result 
    where
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

decryptProperty :: String -> FilePath -> IO (Either String String)
decryptProperty ('{':'C':'L':'E':'A':'R':'}':xs) _ = return $ Right xs 
decryptProperty xs keyFile = decryptString xs keyFile  