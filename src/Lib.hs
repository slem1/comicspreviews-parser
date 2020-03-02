module Lib
    ( 
        downloadWeekReleases
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

logFilePath = "/home/slemoine/dev/workspace/comicspreviews-parser/out.log"


downloadWeekReleases :: String -> FilePath -> ReaderT DC_T.Config IO ()
downloadWeekReleases date path = ask >>= (\config -> lift $ do    
    property <- DC.require config (T.pack "previewsworld_url")
    let url = property ++ "&releaseDate=" ++ date
    request <- parseUrlThrow url
    response <- httpLBS $ request 
    let body = getResponseBody response
    L8.writeFile path body)

-- log' $ T.pack (url ++ ":" ++ (show $ getResponseStatusCode response))        
--withFileLogging logFilePath $ 
 --   log' $ T.pack ("download from " ++ url ++ " to " ++ path)
