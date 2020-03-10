import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char

parser1 :: Parser [(String,[String])]
parser1 = do
	editor <- manyTill anyChar endOfLine
	endOfLine
	value <- try $ many item
	newline
	result <- parser1
	return $ (editor, value):result

--optionMaybe $ sepBy item endOfLine
--case value of
--	Just v -> return $ Just (editor,v)
--	Nothing -> return Nothing
	 
item :: Parser String
item = do 
	r <- many1 (noneOf ['\n'])
	newline
	return r
	

parseIt path = do 
	content <- readFile path
	return $ parse parser1 "" content
	
