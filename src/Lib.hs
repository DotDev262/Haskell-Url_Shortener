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
    , storeShortURL
    , retrieveOriginalURL
    ) where

import Crypto.Random (getRandomBytes)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import Database.SQLite.Simple
import GHC.Generics (Generic)
import qualified Data.Map as Map
import Control.Concurrent.STM (TVar, atomically, readTVar, modifyTVar)

-- SQLite Functionality
data URLMapping = URLMapping String String
    deriving (Show, Generic)

instance FromRow URLMapping where
    fromRow = URLMapping <$> field <*> field

instance FromJSON URLMapping
instance ToJSON URLMapping

generateShortURL :: IO String
generateShortURL = do
    bytes <- getRandomBytes 6
    return $ map (\b -> toEnum (fromEnum b `mod` 26 + fromEnum 'a')) (BS.unpack bytes)

createTable :: Connection -> IO ()
createTable conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS urls (id INTEGER PRIMARY KEY, original_url TEXT, short_url TEXT UNIQUE);"

insertURLMapping :: Connection -> String -> String -> IO ()
insertURLMapping conn original short = do
    execute conn "INSERT INTO urls (original_url, short_url) VALUES (?, ?);" (original, short)

getAllMappings :: Connection -> IO [URLMapping]
getAllMappings conn = do
    query_ conn "SELECT original_url, short_url FROM urls;"

getOriginalURL :: Connection -> String -> IO (Maybe String)
getOriginalURL conn short = do
    rows <- query conn "SELECT original_url FROM urls WHERE short_url = ?;" (Only short)
    return $ case rows of
        [Only orig] -> Just orig
        _ -> Nothing

-- In-Memory Functionality
data UrlEntry = UrlEntry
    { originalUrl :: String
    , shortUrl :: String
    } deriving (Show)

type UrlStore = Map.Map String UrlEntry

storeShortURL :: TVar UrlStore -> String -> String -> IO ()
storeShortURL storeVar original short = atomically $ do
    modifyTVar storeVar (Map.insert short (UrlEntry original short))

retrieveOriginalURL :: TVar UrlStore -> String -> IO (Maybe String)
retrieveOriginalURL storeVar short = atomically $ do
    store <- readTVar storeVar
    return $ originalUrl <$> Map.lookup short store