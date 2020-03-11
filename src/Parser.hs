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

catalogByEditor :: Parser [(String,[[String]])]
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

parseContent :: String -> Either ParseError [(String, [[String]])] 
parseContent content = parse (startTag >> catalogByEditor) "" content

parseFile :: FilePath -> IO [(String, [[String]])]
parseFile filePath = do 
    content <- readFile filePath  
    let result = parseContent content
    case result of 
        Right r -> return r
        Left err -> error $ show err