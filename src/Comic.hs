{-# LANGUAGE OverloadedStrings #-}

module Comic (
    Comic(..)
) where

import Data.Aeson
import Data.Aeson.Types

data Comic = Comic {
    id :: String,
    title :: String,
    price :: String,
    editor :: String
} deriving (Show, Eq)

instance ToJSON Comic where  
    toJSON (Comic id title price editor) =
        object ["id" .= id, "title" .= title, "price" .= price, "editor" .= editor]
    toEncoding (Comic id title price editor) =
        pairs ("id" .= id <> "title" .= title <> "price" .= price <> "editor" .= editor) 


instance FromJSON Comic where
    parseJSON = withObject "Comic" $ \v -> Comic
        <$> v .: "id"
        <*> v .: "title"
        <*> v .: "price"
        <*> v .: "editor"