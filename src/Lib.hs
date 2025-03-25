{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( URLMapping(..)
    , generateShortURL
    , createTable
    , insertURLMapping
    , getAllMappings
    , getOriginalURL
    , UrlEntry(..)
    , UrlStore
    , generateShortCode
    , isValidUrl
    ) where

-- | This module (Lib.hs) contains the core logic for the URL shortener application.
-- | It defines the data structures and functions for interacting with the SQLite database
-- | and the in-memory URL store. It handles URL shortening, database operations,
-- | and in-memory storage.

import Crypto.Random (getRandomBytes) -- | For generating cryptographically secure random bytes.
import Data.Aeson (FromJSON, ToJSON) -- | JSON encoding and decoding.
import qualified Data.ByteString as BS -- | Byte strings for efficient binary data manipulation.
import Database.SQLite.Simple -- | SQLite database interaction.
import GHC.Generics (Generic) -- | Generic programming support.
import Control.Concurrent.STM (TVar, atomically, readTVar, modifyTVar) -- | Software Transactional Memory for concurrent operations.
import Data.Map (Map) -- | Maps for in-memory URL storage.
import qualified Data.Map as Map -- | Maps for in-memory URL storage.
import Network.URI (parseURI) -- | For parsing and validating URIs.


-- SQLite Functionality

-- | Data type to represent a URL mapping (original URL and short URL) for SQLite.
data URLMapping = URLMapping String String
    deriving (Show, Generic)

-- | Instances for SQLite database interaction.
instance FromRow URLMapping where
    fromRow = URLMapping <$> field <*> field

-- | Instances for JSON serialization/deserialization.
instance FromJSON URLMapping
instance ToJSON URLMapping

-- | Generates a short URL string using random bytes.
generateShortURL :: IO String
generateShortURL = do
    bytes <- getRandomBytes 6
    return $ map (\b -> toEnum (fromEnum b `mod` 26 + fromEnum 'a')) (BS.unpack bytes)

-- | Creates the 'urls' table in the SQLite database if it doesn't exist.
createTable :: Connection -> IO ()
createTable conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS urls (id INTEGER PRIMARY KEY, original_url TEXT, short_url TEXT UNIQUE);"

-- | Inserts a new URL mapping into the 'urls' table.
insertURLMapping :: Connection -> String -> String -> IO ()
insertURLMapping conn original short = do
    execute conn "INSERT INTO urls (original_url, short_url) VALUES (?, ?);" (original, short)

-- | Retrieves all URL mappings from the 'urls' table.
getAllMappings :: Connection -> IO [URLMapping]
getAllMappings conn = do
    query_ conn "SELECT original_url, short_url FROM urls;"

-- | Retrieves the original URL for a given short URL from the 'urls' table.
getOriginalURL :: Connection -> String -> IO (Maybe String)
getOriginalURL conn short = do
    rows <- query conn "SELECT original_url FROM urls WHERE short_url = ?;" (Only short)
    return $ case rows of
        [Only orig] -> Just orig
        _ -> Nothing

-- In-Memory Functionality

-- | Data type to represent a URL entry with an optional password for in-memory storage.
data UrlEntry = UrlEntry
    { originalUrl :: String
    , password :: Maybe String
    } deriving (Show)

-- | Type alias for the in-memory URL store (Map of short URL to UrlEntry).
type UrlStore = Map String UrlEntry

-- | Generates a short code and stores the URL entry in the in-memory store.
generateShortCode :: TVar UrlStore -> String -> Maybe String -> IO String
generateShortCode storeVar longUrl pass = atomically $ do
    store <- readTVar storeVar
    let shortCode = "xyz" ++ show (Map.size store + 1)
    let entry = UrlEntry longUrl pass
    modifyTVar storeVar (Map.insert shortCode entry)
    return shortCode


-- Function to validate URLs
-- Checks if a given string is a valid URL using parseURI
isValidUrl :: String -> Bool
isValidUrl url = case parseURI url of
    Just _ -> True
    Nothing -> False