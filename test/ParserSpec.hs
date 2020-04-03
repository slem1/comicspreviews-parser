module ParserSpec where

import Test.Hspec
import Parser
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char

spec :: Spec
spec = describe "test of Parser module" $ do
    it "parse a catalog" $ do
       Right result <- parseFile "test/catalog.txt" 
       result `shouldBe` ("2/19/2020",[("BOOM! STUDIOS",[["OCT191408","BUFFY THE VAMPIRE SLAYER TP VOL 02","$14.99"],["DEC191251","FIREFLY #14 CVR A MAIN ASPINALL","$3.99"]]),("DARK HORSE COMICS",[["DEC190209","BANG #1 (OF 5) CVR A TORRES","$3.99"],["DEC190210","BANG #1 (OF 5) CVR B KINDT","$3.99"]])])
    it "parse start tag" $ do
       let Right result = parse startTag "" "New Releases For 3/3/2020\nblabla\n\nGo to comicshoplocator.com to find a store near you!\n\nPREMIER PUBLISHERS\n\n\n"
       result `shouldBe` "3/3/2020"

