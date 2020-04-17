{-# LANGUAGE OverloadedStrings #-}
module Util (
    logError
    ,decryptProperty
) where

import qualified Control.Logging as LOGGER
import Data.Text
import Crypto

logError :: Text -> IO ()
logError msg = LOGGER.loggingLogger LOGGER.LevelError "" msg

decryptProperty :: String -> FilePath -> IO (Either String String)
decryptProperty ('{':'C':'L':'E':'A':'R':'}':xs) _ = return $ Right xs 
decryptProperty xs keyFile = decryptString xs keyFile  