module Parser (

) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char

garbage :: Parser String
garbage = manyTill anyChar startTag

startTag :: Parser String
startTag = do 
    first <- string "PREMIER PUBLISHERS" 
    newline 
    newline 
    return first

parseContent :: String -> Either ParseError String 
parseContent content = parse startTag "" content

parseFile :: FilePath -> IO ()
parseFile = do 
    content <- readFile 
    let result = parseContent content
    case result of 
        Left r -> putStrLn r
        _ -> error "error parsing"