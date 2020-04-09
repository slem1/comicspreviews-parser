{-# LANGUAGE OverloadedStrings #-}
module Util (
    logError
) where

import qualified Control.Logging as LOGGER
import Data.Text

logError :: Text -> IO ()
logError msg = LOGGER.loggingLogger LOGGER.LevelError "" msg