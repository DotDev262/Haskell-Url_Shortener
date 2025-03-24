{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( URLMapping(..)
    , generateShortURL
    , createTable
    , insertURLMapping
    , getAllMappings
    , getOriginalURL
    ) where

-- Import necessary modules
import Crypto.Random (getRandomBytes)  -- For generating random bytes for short URLs
import Data.Aeson (FromJSON, ToJSON)  -- For JSON serialization and deserialization
import qualified Data.ByteString as BS  -- For working with ByteStrings
import Database.SQLite.Simple  -- For interacting with the SQLite database
import GHC.Generics (Generic)  -- For deriving generic instances for data types

-- Data type representing a URL mapping, with an original URL and a shortened URL
data URLMapping = URLMapping String String
    deriving (Show, Generic)

-- Make the URLMapping type compatible with SQL database rows (fromRow instance)
instance FromRow URLMapping where
    fromRow = URLMapping <$> field <*> field  -- Convert each row to a URLMapping

-- Make the URLMapping type compatible with JSON (FromJSON and ToJSON instances)
instance FromJSON URLMapping
instance ToJSON URLMapping

-- Function to generate a random 6-character string to use as a short URL
generateShortURL :: IO String
generateShortURL = do
    -- Generate 6 random bytes
    bytes <- getRandomBytes 6
    -- Convert each byte into a character (mapped to letters a-z)
    return $ map (\b -> toEnum (fromEnum b `mod` 26 + fromEnum 'a')) (BS.unpack bytes)

-- Function to create the database table for storing URL mappings
createTable :: Connection -> IO ()
createTable conn = do
    -- Execute a SQL statement to create the table if it doesn't exist
    execute_ conn "CREATE TABLE IF NOT EXISTS urls (id INTEGER PRIMARY KEY, original_url TEXT, short_url TEXT UNIQUE);"

-- Function to insert a URL mapping (original and short URLs) into the database
insertURLMapping :: Connection -> String -> String -> IO ()
insertURLMapping conn original short = do
    -- Execute an SQL statement to insert the mapping into the database
    execute conn "INSERT INTO urls (original_url, short_url) VALUES (?, ?);" (original, short)

-- Function to retrieve all URL mappings (original and short URLs) from the database
getAllMappings :: Connection -> IO [URLMapping]
getAllMappings conn = do
    -- Query the database to fetch all URL mappings
    query_ conn "SELECT original_url, short_url FROM urls;"

-- Function to retrieve the original URL from the database based on a short URL
getOriginalURL :: Connection -> String -> IO (Maybe String)
getOriginalURL conn short = do
    -- Query the database to find the original URL for the given short URL
    rows <- query conn "SELECT original_url FROM urls WHERE short_url = ?;" (Only short)
    -- Return the result, which is either a Just with the original URL or Nothing if not found
    return $ case rows of
        [Only orig] -> Just orig  -- Found the original URL
        _ -> Nothing  -- No mapping found for the short URL
