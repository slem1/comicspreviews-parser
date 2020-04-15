{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module CryptMe (
    generatePrivateKey,
    readKey,
    encryptIt,
    decryptIt
)
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import Crypto.Error (CryptoFailable(..), CryptoError(..))

import qualified Crypto.Random.Types as CRT

import Data.ByteArray (ByteArray)
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS 
import System.IO
import Data.ByteString.UTF8

generatePrivateKey :: Int -> IO ()
generatePrivateKey n = do 
  iv :: ByteString <- CRT.getRandomBytes $ blockSize (undefined :: AES256)
  bytes <- CRT.getRandomBytes n   
  h <- openFile "private.key" ReadWriteMode
  BS.hPut h iv
  BS.hPut h bytes   
  hClose h

readKey :: FilePath -> IO (ByteString, ByteString)
readKey path = do 
  h <- openFile path ReadMode
  iv <- BS.hGet h 16
  key <- BS.hGet h 32
  hClose h
  return (iv, key) 
  
myEncrypt :: (BlockCipher c) => IV c -> ByteString -> ByteString -> ByteString
myEncrypt iv key msg = case (cipherInit key) of
  CryptoFailed err -> error $ show err
  CryptoPassed c -> ctrCombine c iv msg
  
encryptIt :: String -> FilePath -> IO ()
encryptIt msg keyPath = do
  (v, k) <- readKey keyPath
  mIV <- initIV v
  case mIV of
    Nothing -> error "Cannot create initialisation vector"
    Just iv -> do 
      BS.writeFile "vanilla" msg'
      encrypted <- return $ myEncrypt iv k msg'
      BS.writeFile "encrypted" encrypted
  where
    msg'= fromString msg 
    initIV :: forall m. (CRT.MonadRandom m) => ByteString -> m (Maybe (IV AES256))
    initIV v = return $ makeIV v

decryptIt :: FilePath -> FilePath -> IO ()
decryptIt password keyPath  = do
  (v, k) <- readKey keyPath
  mIV <- initIV v
  case mIV of
    Nothing -> error "Cannot create initialisation vector"
    Just iv -> do 
      pwd <- BS.readFile password
      decrypted <- return $ myEncrypt iv k pwd
      BS.writeFile "decrypted" decrypted
  where
    initIV :: forall m. (CRT.MonadRandom m) => ByteString -> m (Maybe (IV AES256))
    initIV v = return $ makeIV v

     