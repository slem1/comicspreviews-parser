{-# LANGUAGE OverloadedStrings #-}
module Util (
    logError
    ,decryptProperty
) where

import qualified Control.Logging as LOGGER
import Data.Text
import Crypto

-- |The 'logError' log a text with ERROR log level
logError :: 
    Text        -- ^ The 'text' to log
    -> IO ()    -- ^ The return IO    
logError msg = LOGGER.loggingLogger LOGGER.LevelError "" msg

-- |The 'decryptProperty' decrypts a potentially AES256 encrypted property. If the value the raw value of the
--  property, it should be prefix with {CLEAR}
decryptProperty :: 
    String                          -- ^ The 'property value' argument
    -> FilePath                     -- ^ The 'path' of the private key file
    -> IO (Either String String)    -- ^ The return property value
decryptProperty ('{':'C':'L':'E':'A':'R':'}':xs) _ = return $ Right xs 
decryptProperty xs keyFile = decryptString xs keyFile  