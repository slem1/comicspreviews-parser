import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char

editorCatalog :: Parser [(String,[[String]])]
editorCatalog = do
	editor <- manyTill anyChar endOfLine
	endOfLine
	value <- try $ many item
	sep <- optionMaybe endOfLine
	case sep of
		Nothing -> return [(editor, value)] 
		Just _ -> do
		result <- editorCatalog
		return $ (editor, value):result

--optionMaybe $ sepBy item endOfLine
--case value of
--	Just v -> return $ Just (editor,v)
--	Nothing -> return Nothing
	 
item :: Parser [String]
item = do 
	r <- sepBy1 (many1 $ noneOf ['\n','\t','\r']) $ separator
	endOfLine
	return r	

separator :: Parser Char
separator = char '\t'

parseIt path = do 
	content <- readFile path
	return $ parse editorCatalog "" content
	
