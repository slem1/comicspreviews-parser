{-# LANGUAGE OverloadedStrings #-}

module Comic (
    Comic(..)
) where

import Data.Aeson
import Data.Aeson.Types

data Comic = Comic {
    id :: String,
    title :: String,
    price :: String
} deriving Show

instance ToJSON Comic where  
    toJSON (Comic id title price) =
        object ["id" .= id, "title" .= title, "price" .= price]
    toEncoding (Comic id title price) =
        pairs ("id" .= id <> "title" .= title <> "price" .= price) 


instance FromJSON Comic where
    parseJSON = withObject "Comic" $ \v -> Comic
        <$> v .: "id"
        <*> v .: "title"
        <*> v .: "price"