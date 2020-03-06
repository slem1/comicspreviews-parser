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

items :: Parser [[String]]
items = endBy item endOfLine
    where item = sepBy (many $ noneOf ['\n','\t','\r']) $ separator

parseContent :: String -> Either ParseError [[String]] 
parseContent content = parse (startTag >> editor >> items) "" content

parseFile :: FilePath -> IO [[String]]
parseFile filePath = do 
    content <- readFile filePath  
    let result = parseContent content
    case result of 
        Right r -> return r
        Left err -> error $ show err