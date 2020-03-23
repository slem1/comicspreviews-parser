module Main where
import Lib
import Parser
import qualified Data.Text as T
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import Control.Monad.Trans.Reader
import Comic

main :: IO [[Comic]]
main = do
    config <- loadMainConfig 
    files <- runReaderT (download "2020-03-18") config
    sequence [ case file of 
        Nothing -> return [] 
        Just f -> parseFromCatalog f | file <- files]

loadMainConfig :: IO DC_T.Config
loadMainConfig = DC.load $ [DC.Required "application.properties"] 

parseAFile = parseFile "/Users/slemoine/dev/workspace/comicspreviews-parser/march.txt"