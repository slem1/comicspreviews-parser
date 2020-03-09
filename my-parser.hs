import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char

parser1 :: Parser (Maybe [String])
parser1 = do
	value <- optionMaybe $ sepBy item endOfLine
	case value of
		Just v -> return $ Just v
		Nothing -> return Nothing
	 
item :: Parser String
item = do
	art <- try $ many (noneOf ['\n']) 
	return art

parseIt path = do 
	content <- readFile path
	return $ parse parser1 "" content
	