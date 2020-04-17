module UtilSpec where

import Util
import Test.Hspec

spec :: Spec
spec = describe "test of Util module" $ do
    it "decrypt a clear property" $ do
       Right result <- decryptProperty "{CLEAR}Azerty123" "test/test.key" 
       result `shouldBe` "Azerty123"  
    it "decrypt encrypted property" $ do
       Right result <- decryptProperty "Wi2ttQjJstdu6yqxUQ==" "test/test.key" 
       result `shouldBe` "comicpreviews"       





