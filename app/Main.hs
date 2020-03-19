module Main where
import Lib
import Parser
import qualified Data.Text as T
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import Control.Monad.Trans.Reader

main :: IO [Maybe FilePath]
main = do
    config <- loadMainConfig 
    runReaderT (download "2020-03-18") config

loadMainConfig :: IO DC_T.Config
loadMainConfig = DC.load $ [DC.Required "application.properties"] 

parseAFile = parseFile "/Users/slemoine/dev/workspace/comicspreviews-parser/march.txt"