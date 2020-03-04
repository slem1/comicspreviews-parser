module Parser (
    parseFile
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char

--garbage :: Parser String
--garbage = manyTill anyChar startTag

startTag :: Parser ()
startTag = do 
    garbage <- manyTill anyChar (try $ (string "PREMIER PUBLISHERS"))
    newline
    newline
    newline
    return ()

editor :: Parser String
editor = do
    r <- manyTill anyChar newline 
    newline
    return r

items :: Parser [String]
items = sepBy (manyTill anyChar separator) separator

separator :: Parser Char
separator = char '\t'

parseContent :: String -> Either ParseError [String] 
parseContent content = parse (startTag >> editor >> items) "" content

parseFile :: FilePath -> IO ()
parseFile filePath = do 
    content <- readFile filePath  
   -- putStrLn content
    let result = parseContent content
    case result of 
        Right r -> putStrLn (show r)
        Left err -> error $ show err