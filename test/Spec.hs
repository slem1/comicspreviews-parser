import Test.HUnit
import Lib
import System.IO.Temp
import System.IO

--test1 = TestCase (assertEqual "for (foo 3)," (1,2) (1,3))

main :: IO Counts
main = runTestTT tests

tests = TestList [TestLabel "testDownloadWeekReleases" testDownloadWeekReleases]

testDownloadWeekReleases = TestCase $ do
    result <- downloadAndReadSize
    assertBool "testDownloadWeekReleases" $ result /= 0    
    where 
        downloadAndReadSize = createTempFile >>= \path -> download path >> readSize path
        createTempFile = emptySystemTempFile "tempComicsPreviewsTest" 
        download tempFilePath = downloadWeekReleases "02/19/2020" tempFilePath
        readSize tempFilePath = openFile tempFilePath ReadMode >>= \handle -> hFileSize handle
