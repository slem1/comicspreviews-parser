module ParserSpec where

import Test.Hspec
import Parser

spec :: Spec
spec = describe "Parser test" $ do
    it "ParseContent" $ do
       result <- parseFile "test/catalog.txt" 
       result `shouldBe` [["OCT191408","BUFFY THE VAMPIRE SLAYER TP VOL 02","$14.99"],["DEC191251","FIREFLY #14 CVR A MAIN ASPINALL","$3.99"],[""],["DARK HORSE COMICS"],[""],["DEC190209","BANG #1 (OF 5) CVR A TORRES","$3.99"],["DEC190210","BANG #1 (OF 5) CVR B KINDT","$3.99"]]