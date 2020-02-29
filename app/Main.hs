module Main where
import Lib
import qualified Data.Configurator as DC
import qualified Data.Text as T

main :: IO ()
main = do downloadWeekReleases "02/19/2020" "/home/slemoine/dev/workspace/comicspreviews-parser/march.txt"

loadConfig :: IO ()
loadConfig = do
    config <- load'
    value <- DC.require config (T.pack "previewsworld_url")
    putStrLn value
    where load' = DC.load $ [DC.Required "/home/slemoine/dev/workspace/comicspreviews-parser/application.properties"] 
