import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char

parser1 :: Parser ()
parser1 = endOfLine >> endOfLine >> eof

parseIt path = do 
	content <- readFile path
	return $ parse parser1 "" content
	