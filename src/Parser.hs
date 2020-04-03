module Parser (
    parseFile,    
    startTag
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Data.Bifunctor

type ComicsByEditor = (String, [[String]]) -- ("Marvel", [["SpiderMan", "3.99$"], ["X-men", "3.99$"]])
type Catalog = (String, [ComicsByEditor])  -- ("3/3/2020", [ComicsByEditor])

startTag :: Parser String
startTag = do 
    string "New Releases For "
    date <- manyTill anyChar endOfLine 
    garbage <- manyTill anyChar (try $ (string "PREMIER PUBLISHERS"))
    endOfLine
    endOfLine
    endOfLine
    return date

editor :: Parser String
editor = do
    r <- manyTill anyChar endOfLine
    endOfLine
    return r

catalogByEditor :: Parser [ComicsByEditor]
catalogByEditor = do
	editor <- manyTill anyChar endOfLine
	endOfLine
	items <- try $ many item
	sep <- optionMaybe endOfLine
	case sep of
		Nothing -> return [(editor, items)] --must be EOF case
		Just _ -> catalogByEditor >>= \result -> return $ (editor, items):result
	 
item :: Parser [String]
item = do 
	r <- sepBy1 (many1 $ noneOf ['\n','\t','\r']) $ separator
	endOfLine
	return r	
    where separator = char '\t'

parseFile :: FilePath -> IO (Either String Catalog)
parseFile filePath = do 
    content <- readFile filePath  
    let result = parse contentParser "" content        
    return $ first show result
    where        
        contentParser :: Parser Catalog
        contentParser = do
            date <- startTag
            editorCatalogs<- catalogByEditor
            return (date, editorCatalogs)