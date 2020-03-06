module LibSpec where

import Test.HUnit
import Lib
import System.IO.Temp
import System.IO
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import Control.Monad.Trans.Reader
import Test.Hspec

spec :: Spec
spec = describe "test" $ do
     it "returns a positive number when given a negative number" $ do
        result <- testDownloadWeekReleases
        result `shouldSatisfy` \a -> a > 0

testDownloadWeekReleases :: IO Integer
testDownloadWeekReleases = do
    config <- DC.load $ [DC.Required "application.properties"]
    tmp <- createTempFile
    runReaderT (download tmp) config 
    readSize tmp
    where 
        createTempFile = emptySystemTempFile "tempComicsPreviewsTest" 
        download tempFilePath = downloadWeekReleases "02/19/2020" tempFilePath
        readSize tempFilePath = openFile tempFilePath ReadMode >>= \handle -> hFileSize handle
