module Main where
import Lib
import qualified Data.Text as T

main :: IO ()
main = do downloadWeekReleases "02/19/2020" "/home/slemoine/dev/workspace/comicspreviews-parser/march.txt"


