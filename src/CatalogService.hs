{-# LANGUAGE OverloadedStrings #-}
module CatalogService
    ( 
        download,
        downloadWeekReleases,
        parseFromCatalog,
        addCatalog,
        releaseDay,
        releaseDays
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
import Database.PostgreSQL.Simple
import Data.Int
import Parser
import Comic

type ComicsByEditor = (String, [[String]]) -- ("Marvel", [["SpiderMan", "3.99$"], ["X-men", "3.99$"]])
type Catalog = (String, [ComicsByEditor])  -- ("3/3/2020", [ComicsByEditor])

download :: Day -> ReaderT DC_T.Config IO [Maybe (Day, FilePath)]
download date = ask >>= (\config -> lift $ do 
    url <- DC.require config (T.pack "previewsworld_url")
    outputDir <- DC.require config (T.pack "output_dir")
    sequence $ [ case releaseDate of
                    Nothing -> return Nothing
                    Just d -> downloadWeekReleases url d outputDir |  releaseDate <- (releaseDays date)]
    )
-- |The 'downloadWeekReleases' download the catalog from 'url' with release day 'date' to directory 'outputDir'
downloadWeekReleases :: String       -- ^ The 'url' argument
    -> Day                           -- ^ The 'date' argument
    -> FilePath                      -- ^ The 'outputDir' argument 
    -> IO (Maybe (Day, FilePath))    -- ^ return the downloaded catalog filepath
downloadWeekReleases url date outputDir =  do
    let queryDate = formatTime defaultTimeLocale "%m/%d/%Y" date
    let fileNameDate = formatTime defaultTimeLocale "%Y-%m-%d" date 
    let reqUrl = url ++ "&releaseDate=" ++ queryDate
    let outputPath = outputDir ++ "catalog-" ++ fileNameDate ++ ".txt"
    request <- parseUrlThrow reqUrl
    response <- httpLBS $ request 
    let body = getResponseBody response
    L8.writeFile outputPath body
    return $ Just (date, outputPath)

parseFromCatalog :: FilePath -> Day -> IO (Either String [Comic])
parseFromCatalog path date = do     
    result <- parseFile path 
    return $ do 
        (parsedDate, comicsByEditor) <- result
        let catalogParsedDate = (parseTimeOrError True defaultTimeLocale "%-m/%-d/%Y" parsedDate) :: Day        
        if catalogParsedDate == date then
            Right $ foldr (\e acc -> (mapFromEditor e) ++ acc ) [] comicsByEditor
        else
            Left "Release date in the catalog content does not match"    

mapFromEditor :: (String, [[String]]) -> [Comic]
mapFromEditor (editor, x:[]) =  [mapToComic x editor]
mapFromEditor (editor, x:xs) =  (mapToComic x editor) : (mapFromEditor (editor,xs))

mapToComic :: [String] -> String -> Comic
mapToComic [id, title, price] = Comic id title price

-- |The 'releaseDay' function returns the release day in the week of referenceDate.
releaseDay :: Day        -- ^ The 'referenceDate' argument
    -> Maybe Day         -- ^ The return date
releaseDay date = do     
    let (year, week, day) = toWeekDate date
    fromWeekDateValid year week 3    

nextReleaseDay :: Maybe Day -> Maybe Day
nextReleaseDay d = (addDays 7) <$> d

-- |The 'releaseDays' function returns the release day in the week of the given date
-- and the week+1 release date.
releaseDays :: Day      -- ^ The 'referenceDate' argument
    -> [Maybe Day]           -- ^ The return release dates
releaseDays d = let d0 = releaseDay d
    in [d0, nextReleaseDay d0]

-- |The 'addCatalog' function insert a complete catalog of comics in database
addCatalog :: Connection    -- ^ The 'connection' to the database argument
    -> (Day, FilePath)      -- ^ The 'catalog' description
    -> [Comic] -> IO Int64  -- ^ The 'catalog' content
addCatalog conn (date, path) comics = 
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


-- log' $ T.pack (url ++ ":" ++ (show $ getResponseStatusCode response))        
--withFileLogging logFilePath $ 
 --   log' $ T.pack ("download from " ++ url ++ " to " ++ path)