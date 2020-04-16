module CryptoSpec where

import Test.Hspec
import CatalogService
import System.IO.Temp
import System.IO
import Crypto

spec :: Spec
spec = describe "Crypto module tests" $ do
    it "should read single digit date from comicspreviews 2" $ do   
        withSystemTempDirectory "previewsworld-test" (\path -> do 
            let keyFile = path ++ "/private.key.test"
            let raw = "Azerty@123"
            generateKey $ keyFile
            encrypted <- encryptString raw keyFile
            Right decrypted <- decryptString encrypted keyFile
            decrypted `shouldBe` raw)  



