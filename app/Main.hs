module Main where
import Lib
import qualified Data.Text as T
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import Control.Monad.Trans.Reader

main :: IO ()
main = do
    config <- loadMainConfig 
    outputDir <- DC.require config (T.pack "output_dir")
    let outputFile = outputDir ++ "20201902-catalog.txt" 
    runReaderT (downloadWeekReleases "02/19/2020" outputFile) config

loadMainConfig :: IO DC_T.Config
loadMainConfig = DC.load $ [DC.Required "application.properties"] 