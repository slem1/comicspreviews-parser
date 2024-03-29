{-# LANGUAGE OverloadedStrings #-}
module CatalogService
    (         
        downloadReleases,        
        parseFromCatalog,
        addCatalog,
        releaseDay,
        releaseDays,
        isCatalogExist
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Control.Logging as LOGGER
import Network.HTTP.Simple
import Network.HTTP.Conduit
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
import Control.Exception
import qualified Util as U

type ComicsByEditor = (String, [[String]]) -- ("Marvel", [["SpiderMan", "3.99$"], ["X-men", "3.99$"]])
type Catalog = (String, [ComicsByEditor])  -- ("3/3/2020", [ComicsByEditor])

-- |The 'downloadWeekReleases' download the catalog from 'url' with release day 'date' to directory 'outputDir'
downloadReleases :: String       -- ^ The 'url' argument
    -> Day                           -- ^ The 'date' argument
    -> FilePath                      -- ^ The 'outputDir' argument 
    -> IO (Maybe (Day, FilePath))    -- ^ return the downloaded catalog filepath
downloadReleases url date outputDir = catch (download url date outputDir) httpExceptionHandler where        
        httpExceptionHandler :: HttpException -> IO (Maybe a)
        httpExceptionHandler e = (U.logError $ T.pack . show $ e) >> return Nothing
        download url date outputDir =  do
            let queryDate = formatTime defaultTimeLocale "%m/%d/%Y" date
            let fileNameDate = formatTime defaultTimeLocale "%Y-%m-%d" date 
            let reqUrl = url ++ "&releaseDate=" ++ queryDate
            let outputPath = outputDir ++ "catalog-" ++ fileNameDate ++ ".txt"
            request <- parseRequestThrow reqUrl
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

-- |The 'releaseDays' function returns the release day date (i.e wednesday) in the week according to given date
-- and all the previous or next release dates.
releaseDays :: 
    Day                      -- ^ The 'referenceDate' argument    
    -> Integer               -- ^ The 'offset of week' argument 
    -> Maybe [Day]           -- ^ The return release dates
releaseDays date offset =
    releaseDay date >>= \d -> return $ (offsetWeekRelease <$> (array offset)) <*> pure d where
    offsetWeekRelease offset d = addDays (7 * offset) d
    array offset 
       | offset < 0 = [0,-1.. offset]   
       | offset > 0 = [0,1.. offset]
       | otherwise = [0]         

-- |The 'releaseDay' function returns the release day in the week of referenceDate.
releaseDay :: Day        -- ^ The 'referenceDate' argument
    -> Maybe Day         -- ^ The return date
releaseDay date = do     
    let (year, week, day) = toWeekDate date
    fromWeekDateValid year week 3    


-- |The 'addCatalog' function insert a complete catalog of comics in database
addCatalog :: Connection    -- ^ The 'connection' to the database argument
    -> (Day, FilePath)      -- ^ The 'catalog' description
    -> [Comic] -> IO Int64  -- ^ The 'catalog' content
addCatalog conn (date, path) comics = 
    let dateStr = formatTime defaultTimeLocale "%Y-%m-%d" date
    in withTransaction conn $ insertCatalogLine conn (date, path) >>= \(Only idCatalog)-> insertComicsLines conn comics idCatalog

isCatalogExist :: Connection -> Day -> IO Bool
isCatalogExist conn date = do
    rs <- (query conn "SELECT 1 FROM comicspreviews.t_catalog WHERE date_release = ?" (Only date)) :: IO [(Only Int)]    
    case rs of
        [] -> return False
        _ -> return True

insertCatalogLine :: Connection -> (Day, FilePath) -> IO (Only Int)
insertCatalogLine conn catalog = do
    [x] <- query conn "INSERT INTO comicspreviews.t_catalog (date_release,filepath) VALUES (?,?) RETURNING id_t_catalog" catalog
    return x

insertComicsLines :: Connection -> [Comic] -> Int -> IO Int64
insertComicsLines conn comics idCatalog = executeMany conn "INSERT INTO comicspreviews.t_comic (id_t_catalog, reference, title, price, editor) VALUES (?, ?, ?, ?, ?)" (mapper <$> comics)
    where
        mapper (Comic id title price editor) = (idCatalog, id, title, price, editor)