module Lib
    ( 
        download,
        downloadWeekReleases,
        releaseDay,
        releaseDays,
        parseFromCatalog
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Control.Logging
import qualified Data.Text as T
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Parser
import Comic

logFilePath = "/home/slemoine/dev/workspace/comicspreviews-parser/out.log"

download :: String -> ReaderT DC_T.Config IO [Maybe FilePath]
download date = ask >>= (\config -> lift $ do 
    url <- DC.require config (T.pack "previewsworld_url")
    outputDir <- DC.require config (T.pack "output_dir")
    sequence $ [ case releaseDate of
                    Nothing -> return Nothing
                    Just d -> downloadWeekReleases url d outputDir |  releaseDate <- (releaseDays date)]
    )
-- |The 'downloadWeekReleases' download the catalog from 'url' with release day 'date' to directory 'outputDir'
downloadWeekReleases :: String -- ^ The 'url' argument
    -> Day                     -- ^ The 'date' argument
    -> FilePath                -- ^ The 'outputDir' argument 
    -> IO (Maybe FilePath)     -- ^ return the downloaded catalog filepath
downloadWeekReleases url date outputDir =  do
    let queryDate = formatTime defaultTimeLocale "%m/%d/%Y" date
    let fileNameDate = formatTime defaultTimeLocale "%Y-%m-%d" date 
    let reqUrl = url ++ "&releaseDate=" ++ queryDate
    let outputPath = outputDir ++ "catalog-" ++ fileNameDate ++ ".txt"
    request <- parseUrlThrow reqUrl
    response <- httpLBS $ request 
    let body = getResponseBody response
    L8.writeFile outputPath body
    return $ Just outputPath

parseFromCatalog :: FilePath -> IO [Comic]
parseFromCatalog path = do 
    --IO =[(editor, [[STRING]])]
    result <- parseFile path
    return $ foldr (\e acc -> (mapFromEditor e) ++ acc ) [] result

mapFromEditor :: (String, [[String]]) -> [Comic]
mapFromEditor (editor, x:[]) =  [mapToComic x editor]
mapFromEditor (editor, x:xs) =  (mapToComic x editor) : (mapFromEditor (editor,xs))

mapToComic :: [String] -> String -> Comic
mapToComic [id, title, price] = Comic id title price

-- log' $ T.pack (url ++ ":" ++ (show $ getResponseStatusCode response))        
--withFileLogging logFilePath $ 
 --   log' $ T.pack ("download from " ++ url ++ " to " ++ path)

-- |The 'releaseDay' function returns the release day in the week of referenceDate.
releaseDay :: String     -- ^ The 'referenceDate' argument
    -> Maybe Day         -- ^ The return date
releaseDay referenceDate = do 
    date <- parseTimeM True defaultTimeLocale "%Y-%m-%d" referenceDate  
    let (year, week, day) = toWeekDate date
    fromWeekDateValid year week 3    

nextReleaseDay :: Maybe Day -> Maybe Day
nextReleaseDay d = (addDays 7) <$> d

-- |The 'releaseDays' function returns the release day in the week of the given date
-- and the week+1 release date.
releaseDays :: String ->  -- ^ The 'referenceDate' argument
    [Maybe Day]           -- ^ The return release dates
releaseDays d = let d0 = releaseDay d
    in [d0, nextReleaseDay d0]