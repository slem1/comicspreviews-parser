module CatalogServiceSpec where

import Test.HUnit
import CatalogService
import System.IO.Temp
import System.IO
import Data.Text as T
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import Control.Monad.Trans.Reader
import Test.Hspec
import Data.Time.Format
import Data.Time.Calendar
import Comic

spec :: Spec
spec = describe "test" $ do
     it "returns a positive number when given a negative number" $ do
        result <- testDownloadWeekReleases
        result `shouldSatisfy` \a -> a > 0
     it "should return the day of release" $ do
        let ref = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2020-06-30"
        let Just result = releaseDay ref
        let expected = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2020-07-01" 
        result `shouldBe` expected 
     it "should return the 2-week days of release" $ do
        let ref = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2020-12-31"
        let [d0, d1] = releaseDays ref
        let expectedd0 = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2020-12-30"
        let expectedd1 =  parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2021-01-06"
        [d0, d1] `shouldBe` [Just expectedd0, Just expectedd1]

     it "should create comics from catalog" $ do
        let date = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2020-02-19"
        Right result <- parseFromCatalog "test/catalog.txt" date   
        result `shouldBe` [Comic {Comic.id = "OCT191408", title = "BUFFY THE VAMPIRE SLAYER TP VOL 02", price = "$14.99", editor = "BOOM! STUDIOS"},Comic {Comic.id = "DEC191251", title = "FIREFLY #14 CVR A MAIN ASPINALL", price = "$3.99", editor = "BOOM! STUDIOS"},Comic {Comic.id = "DEC190209", title = "BANG #1 (OF 5) CVR A TORRES", price = "$3.99", editor = "DARK HORSE COMICS"},Comic {Comic.id = "DEC190210", title = "BANG #1 (OF 5) CVR B KINDT", price = "$3.99", editor = "DARK HORSE COMICS"}]

     it "should read single digit date from comicspreviews 1" $ do   
        let d0 = (parseTimeOrError True defaultTimeLocale "%-m/%-d/%Y" "3/3/2020") :: Day
        let expected = (parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2020-03-03") :: Day
        d0 `shouldBe` expected

     it "should read single digit date from comicspreviews 2" $ do   
        let d0 = (parseTimeOrError True defaultTimeLocale "%-m/%-d/%Y" "2/19/2020") :: Day
        let expected = (parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2020-02-19") :: Day
        d0 `shouldBe` expected

testDownloadWeekReleases :: IO Integer
testDownloadWeekReleases = do
   let date = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" "2020-03-18" 
   config <- DC.load $ [DC.Required "application.properties"]
   url <- DC.require config (T.pack "previewsworld_url")
   withSystemTempDirectory "previewsworld-test" (\path -> do 
      Just (_,file) <- downloadWeekReleases url date path
      readSize file)
   where 
      readSize tempFilePath = openFile tempFilePath ReadMode >>= \handle -> hFileSize handle
