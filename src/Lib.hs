module Lib
    ( 
        downloadWeekReleases
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Control.Logging
import qualified Data.Text as T

previewsworldUrl = "https://www.previewsworld.com/NewReleases/Export?format=txt"
logFilePath = "/home/slemoine/dev/workspace/comicspreviews-parser/out.log"

downloadWeekReleases :: String -> FilePath -> IO ()
downloadWeekReleases date path = 
    --withFileLogging logFilePath $ 
    do
    let url = previewsworldUrl ++ "&releaseDate=" ++ date
    request <- parseUrlThrow url
    response <- httpLBS $ request 
   -- log' $ T.pack (url ++ ":" ++ (show $ getResponseStatusCode response))
    let body = getResponseBody response
    L8.writeFile path body
 --   log' $ T.pack ("download from " ++ url ++ " to " ++ path)