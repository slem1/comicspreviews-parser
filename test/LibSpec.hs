module LibSpec where

import Test.HUnit
import Lib
import System.IO.Temp
import System.IO
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import Control.Monad.Trans.Reader
import Test.Hspec
import Data.Time.Format

spec :: Spec
spec = describe "test" $ do
     it "returns a positive number when given a negative number" $ do
        result <- testDownloadWeekReleases
        result `shouldSatisfy` \a -> a > 0
     it "should return the day of release" $ do
        let Just result = releaseDay "2020-06-30"
        let expected = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2020-07-01" 
        result `shouldBe` expected 
     it "should return the 2-week days of release" $ do
        let [d0, d1] = releaseDays "2020-12-31"
        let expectedd0 = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2020-12-30"
        let expectedd1 =  parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2021-01-06"
        [d0, d1] `shouldBe` [Just expectedd0, Just expectedd1]


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
