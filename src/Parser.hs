module Parser (
    parseFile,
    parseContent
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char

startTag :: Parser ()
startTag = do 
    garbage <- manyTill anyChar (try $ (string "PREMIER PUBLISHERS"))
    endOfLine
    endOfLine
    endOfLine
    return ()

editor :: Parser String
editor = do
    r <- manyTill anyChar endOfLine
    endOfLine
    return r

separator :: Parser Char
separator = char '\t'

parseEditorCatalog :: Parser (String, [[String]])
parseEditorCatalog = do
    ed1 <- editor
    it1 <- item
    endOfLine
    it2 <- item
    endOfLine
    endOfLine
    ed2 <- editor
    it21 <- item
    endOfLine
    it22 <- item
    endOfLine
    endOfLine

    return (ed1, [it1, it2])
    where item = sepBy (many $ noneOf ['\n','\t','\r']) $ separator
    

items :: Parser [[String]]
items = endBy item endOfLine
    where item = sepBy (many $ noneOf ['\n','\t','\r']) $ separator

--parseContent :: String -> Either ParseError [[String]] 
--parseContent content = parse (startTag >> editor >> items) "" content
parseContent :: String -> Either ParseError (String, [[String]]) 
parseContent content = parse (startTag >> parseEditorCatalog) "" content

--parseFile :: FilePath -> IO [[String]]
parseFile :: FilePath -> IO (String, [[String]])
parseFile filePath = do 
    content <- readFile filePath  
    let result = parseContent content
    case result of 
        Right r -> return r
        Left err -> error $ show err