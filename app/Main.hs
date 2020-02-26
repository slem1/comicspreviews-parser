module Main where
{-# LANGUAGE OverloadedStrings #-}
import Lib
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
import Network.HTTP.Conduit

main :: IO ()
main = do downloadWeekReleases "02/19/2020" "/Users/slemoine/dev/workspace/comicspreviews-parser/march.txt"

previewsworldUrl = "https://www.previewsworld.com/NewReleases/Export?format=txt"

downloadWeekReleases :: String -> FilePath -> IO ()
downloadWeekReleases date path = do
    request <- parseRequest (previewsworldUrl ++ "&releaseDate=" ++ date)
    response <- httpLBS $ request 
    let body = getResponseBody response
    L8.writeFile path body

