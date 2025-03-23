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
        , "<head>"
        , "<title>URL Shortener</title>"
        , "<style>"
        , "* {"
        , "    margin: 0;"
        , "    padding: 0;"
        , "    box-sizing: border-box;"
        , "    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;"
        , "}"
        , "body {"
        , "    min-height: 100vh;"
        , "    background: linear-gradient(135deg, #f0f4ff 0%, #e6eeff 100%);"
        , "    display: flex;"
        , "    justify-content: center;"
        , "    align-items: center;"
        , "    padding: 20px;"
        , "}"
        , ".container {"
        , "    background: white;"
        , "    padding: 2rem;"
        , "    border-radius: 12px;"
        , "    box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);"
        , "    width: 100%;"
        , "    max-width: 500px;"
        , "}"
        , "h1 {"
        , "    color: #2d3748;"
        , "    text-align: center;"
        , "    margin-bottom: 2rem;"
        , "    font-size: 1.75rem;"
        , "}"
        , ".input-section {"
        , "    display: flex;"
        , "    gap: 1rem;"
        , "    margin-bottom: 1.5rem;"
        , "}"
        , "input {"
        , "    flex: 1;"
        , "    padding: 0.75rem 1rem;"
        , "    border: 2px solid #e2e8f0;"
        , "    border-radius: 8px;"
        , "    font-size: 1rem;"
        , "    transition: all 0.2s ease;"
        , "}"
        , "input:focus {"
        , "    outline: none;"
        , "    border-color: #4299e1;"
        , "    box-shadow: 0 0 0 3px rgba(66, 153, 225, 0.15);"
        , "}"
        , "button {"
        , "    padding: 0.75rem 1.5rem;"
        , "    background-color: #4299e1;"
        , "    color: white;"
        , "    border: none;"
        , "    border-radius: 8px;"
        , "    cursor: pointer;"
        , "    font-size: 1rem;"
        , "    transition: background-color 0.2s ease;"
        , "}"
        , "button:hover {"
        , "    background-color: #3182ce;"
        , "}"
        , ".output-box {"
        , "    background-color: #f7fafc;"
        , "    border: 2px solid #e2e8f0;"
        , "    border-radius: 8px;"
        , "    padding: 1rem;"
        , "    min-height: 60px;"
        , "} "
         -- Output text styling
         ,"#outputText {"
         ,"   color: #4a5568;" 
         ,"   word-break: break-all;" 
         ,"   line-height: 1.5;" 
         ," }" 
         -- Media query for responsive design
         ,"@media (max-width: 480px) {"
         ,"   .input-section {"
         ,"       flex-direction: column;" 
         ,"   }" 
         ,"   button {"
         ,"       width: 100%;" 
         ,"   }" 
         ," }" 
         -- Closing style tag
         ,"</style>"
         -- Closing head tag
         ,"</head>"
         -- Body content starts here
         ,"<body>"
         -- Container div for better layout
         ,"<div class='container'>"
         ,"<h1>URL Shortener</h1>"
         ,"<div class='input-section'>"
         ,"<input type='text' id='urlInput' placeholder='Enter your URL here'/>"
         ,"<button onclick='shortenUrl()'>Shorten URL</button>"
         ,"</div>"
         ,"<div id='result' class='output-box'>"
         , previousUrls 
         ,"</div>"
         ,"<script>"
         ,"function shortenUrl() {"
         ,"   var url = document.getElementById('urlInput').value;"
         ,"   fetch('/shorten?url=' + encodeURIComponent(url), { method: 'POST' })"
         ,"     .then(response => response.json())"
         ,"     .then(data => {"
         ,"       document.getElementById('result').innerHTML = 'Shortened URL: <a href=\"' + data.short_url + '\">' + data.short_url + '</a>'; "
         ,"     });"
         ," }"
         ,"</script>"
          -- Closing container div and body tag
          ,"</div>"
          ,"</body>"
          -- Closing html tag
          ,"</html>"
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