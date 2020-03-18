module Lib
    ( 
        download,
        releaseDay,
        releaseDays,
        fromCatalog
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
import Network.HTTP.Conduit
--import Control.Logging
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


download :: String -> ReaderT DC_T.Config IO [()]
download date = ask >>= (\config -> lift $ do 
    url <- DC.require config (T.pack "previewsworld_url")
    outputDir <- DC.require config (T.pack "output_dir")
    sequence $ [ downloadWeekReleases url releaseDate outputDir |  releaseDate <- (releaseDays date)]
    )
    
downloadWeekReleases :: String -> Maybe Day -> String -> IO ()
downloadWeekReleases url date outputDir = 
    case date of
        Nothing -> return ()
        Just d -> do
            let queryDate = formatTime defaultTimeLocale "%m/%d/%Y" d 
            let fileNameDate = formatTime defaultTimeLocale "%Y-%m-%d" d 
            let reqUrl = url ++ "&releaseDate=" ++ queryDate
            let outputPath = outputDir ++ "catalog" ++ fileNameDate
            request <- parseUrlThrow reqUrl
            response <- httpLBS $ request 
            let body = getResponseBody response
            L8.writeFile outputPath body

fromCatalog :: FilePath -> IO [Comic]
fromCatalog path = do 
    --IO =[(editor, [[STRING]])]
    result <- parseFile path
    return $ foldr (\e acc -> (fromEditorCatalog e) ++ acc ) [] result

fromEditorCatalog :: (String, [[String]]) -> [Comic]
fromEditorCatalog (editor, x:[]) =  [mapToComic x editor]
fromEditorCatalog (editor, x:xs) =  (mapToComic x editor) : (fromEditorCatalog (editor,xs))

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

--getDates :: String -> [Maybe String]
--getDates date = map' <$> (releaseDays date) 
--    where
--        map' :: Maybe Day -> Maybe String
--        map' day = day >>= (\d -> return $ formatMyDate d) 
