{-# LANGUAGE OverloadedStrings #-}

import Crypto.Random (getRandomBytes)
import Data.Aeson (object, (.=))
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as T
import Database.SQLite.Simple
import Web.Scotty

data URLMapping = URLMapping String String

instance FromRow URLMapping where
  fromRow = URLMapping <$> field <*> field

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

-- Function to get all URL mappings from the database
getAllMappings :: Connection -> IO [URLMapping]
getAllMappings conn = query_ conn "SELECT original_url, short_url FROM urls;"

getOriginalURL :: Connection -> String -> IO (Maybe String)
getOriginalURL conn short = do
  rows <- query conn "SELECT original_url FROM urls WHERE short_url = ?;" (Only short)
  return $ case rows of
    [Only orig] -> Just orig
    _ -> Nothing

serveHTML :: Connection -> ActionM ()
serveHTML conn = do
  mappings <- liftIO $ getAllMappings conn -- Fetch all mappings from the database
  let previousUrls = T.concat [ "<p>Previous Shortened URLs:</p><ul>"
                              , T.concat [ T.pack ("<li><a href=\"http://localhost:3000/" ++ short ++ "\">" ++ short ++ "</a> -> " ++ original ++ "</li>")
                                          | URLMapping original short <- mappings ]
                              , "</ul>" ]
  
  html $ T.concat 
      [ "<html>"
      , "<head><title>URL Shortener</title></head>"
      , "<body>"
      , "<h1>URL Shortener</h1>"
      , "<input type='text' id='urlInput' placeholder='Enter your URL here'/>"
      , "<button onclick='shortenUrl()'>Shorten URL</button>"
      , "<div id='result'></div>"
      , previousUrls -- Include previous shortened URLs here
      , "<script>"
      , "function shortenUrl() {"
      , "  var url = document.getElementById('urlInput').value;"
      , "  fetch('/shorten?url=' + encodeURIComponent(url), { method: 'POST' })"
      , "    .then(response => response.json())"
      , "    .then(data => {"
      , "      document.getElementById('result').innerHTML = 'Shortened URL: <a href=\"' + data.short_url + '\">' + data.short_url + '</a>'; "
      , "    });"
      , "}"
      , "</script>"
      , "</body>"
      , "</html>"
      ]

main :: IO ()
main = do
  conn <- open "urls.db"
  createTable conn

  scotty 3000 $ do
    get "/" $ serveHTML conn -- Pass the connection explicitly here

    post "/shorten" $ do
      original <- queryParam "url" :: ActionM String 
      short <- liftIO generateShortURL

      liftIO $ insertURLMapping conn original short

      json $ object ["short_url" .= ("http://localhost:3000/" ++ short)]

    get "/:short" $ do
      short <- pathParam "short" :: ActionM String 
      maybeOriginal <- liftIO $ getOriginalURL conn short

      case maybeOriginal of
        Just original -> redirect (T.pack original) 
        Nothing -> text "Short URL not found"
