module Lib
    ( 
        downloadWeekReleases
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
import Network.HTTP.Conduit

previewsworldUrl = "https://www.previewsworld.com/NewReleases/Export?format=txt"

downloadWeekReleases :: String -> FilePath -> IO ()
downloadWeekReleases date path = do
    request <- parseUrlThrow (previewsworldUrl ++ "&releaseDate=" ++ date)
    response <- httpLBS $ request 
    let body = getResponseBody response
    L8.writeFile path body