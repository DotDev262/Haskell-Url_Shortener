{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Web.Scotty
import Database.SQLite.Simple
import qualified Data.Text.Lazy as T
import Data.Aeson
import Network.Wai.Middleware.Cors (simpleCors)

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
        , "   var url = document.getElementById('urlInput').value;"
        , "   fetch('/shorten?url=' + encodeURIComponent(url), { method: 'POST' })"
        , "     .then(response => response.json())"
        , "     .then(data => {"
        , "       document.getElementById('result').innerHTML = 'Shortened URL: <a href=\"' + data.short_url + '\">' + data.short_url + '</a>'; "
        , "     });"
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
        middleware simpleCors -- Enable CORS for local development
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