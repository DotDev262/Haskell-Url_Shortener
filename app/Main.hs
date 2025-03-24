{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Web.Scotty
import Database.SQLite.Simple
import qualified Data.Text.Lazy as T
import Data.Aeson
import Network.Wai.Middleware.Cors (simpleCors)
import Network.URI (parseURI)

-- Function to validate URLs
isValidUrl :: String -> Bool
isValidUrl url = case parseURI url of
    Just _  -> True
    Nothing -> False

serveHTML :: Connection -> ActionM ()
serveHTML conn = do
    mappings <- liftIO $ getAllMappings conn -- Fetch all mappings from the database
    let previousUrls = T.concat [ "<p>Shortened URLs:</p>"
                                 , "<div class='url-list'>"
                                 , T.concat [ "<div class='url-item'><a href=\"http://localhost:3000/" <> T.pack short <> "\" class='short-url'>" <> T.pack short <> "</a> <span class='arrow'>→</span> <span class='original-url'>" <> T.pack original <> "</span></div>"
                                              | URLMapping original short <- mappings ]
                                 , "</div>" ]

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
        , "    max-width: 600px;"
        , "}"
        , "h1 {"
        , "    color: #2d3748;"
        , "    text-align: center;"
        , "    margin-bottom: 1.5rem;"
        , "    font-size: 2rem;"
        , "}"
        , ".input-section {"
        , "    display: flex;"
        , "    gap: 1rem;"
        , "    margin-bottom: 1.5rem;"
        , "}"
        , ".url-list {"
        , "   margin-top: 1rem;"
        , "   border-top: 2px solid #e2e8f0;"
        , "   padding-top: 1rem;"
        , "}"
        , ".url-item {"
        , "   padding: 0.5rem 0;"
        , "   display: flex;"
        , "   justify-content: space-between;"
        , "   align-items: center;"
        , "   border-bottom: 1px solid #e2e8f0;"
        ," }"
         -- Short URL link styles
         ," .short-url {"
         ,"   color: #4299e1;" 
         ,"   text-decoration: none;" 
         ,"   font-weight: bold;" 
         ," }" 
         -- Short URL hover effect
         ," .short-url:hover {"
         ,"   text-decoration: underline;" 
         ," }" 
         -- Original URL styles
         ," .original-url {"
         ,"   color: #4a5568;" 
         ," }" 
         -- Arrow styles
         ," .arrow {"
         ,"   margin: 0 10px;" 
         ,"   font-weight:bold;" 
         ," }" 
         -- Input and button styles (same as before)
         ,"input {"
         ,"   flex: 1;" 
         ,"   padding:.75rem ;"  
         ,"   border: 2px solid #e2e8f0;" 
         ,"   border-radius: 8px;"  
         ,"   font-size:1rem;"  
         ,"   transition:.3s;"  
         ," }"  
          -- Input focus styles
          ,"input:focus {"
          ,"   outline:none;"  
          ,"   border-color:#4299e1;"  
          ,"   box-shadow:0 0 0 3px rgba(66,153,225,0.15);"  
          ," }"  
          -- Button styles
          ,"button {"
          ,"   padding:.75rem 1.5rem;"  
          ,"   background-color:#4299e1;"  
          ,"   color:white;"  
          ,"   border-radius:8px;"  
          ,"   border:none;"  
          ,"   cursor:pointer;"  
          ,"   font-size:1rem;"  
          ,"   transition:.3s;"  
          ," }"  
          -- Button hover effect
          ,"button:hover {"
          ,"   background-color:#3182ce;"  
          ," }"  
          -- Output box styles
          ,"#result {"
          ,"   background-color:#f7fafc;"  
          ,"   border-radius:8px;"  
          ,"   padding:.75rem;"  
          ,"   margin-top:.5rem;"  
          ," }"  
           -- Media query for responsive design
           ,"@media (max-width:480px) {"
           ,"   .input-section {"
           ,"       flex-direction: column;"  
           ,"   }"  
           ,"   button {"
           ,"       width:100%;"  
           ,"       margin-top:.5rem;"   
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
           -- Input section for URL entry
           ,"<div class='input-section'>"
           ,"<input type='text' id='urlInput' placeholder='Enter your URL here' aria-label='URL input'/>"
           ,"<button onclick='shortenUrl()'>Shorten URL</button>"
           ,"</div>"
           -- Result output section
           ,"<div id='result' class='output-box'>"
           , previousUrls
           ,"</div>"
           -- JavaScript for URL shortening functionality
           ,"<script>"
           ,"function shortenUrl() {"
           ,"const urlInput = document.getElementById(\"urlInput\").value.trim();"
           ,"const resultElement = document.getElementById(\"result\");"

           ,"function isValidUrl(string) {"
           ,"    try {"
           ,"       new URL(string);"
           ,"       return true;"
           ,"    } catch (_) {"
           ,"       return false;"
           ,"    }"
           ,"}"

           ,"if (!urlInput) {"
           ,"    alert(\"Please enter a URL!\");"
           ,"    return;"
           ,"}"

           ,"if (!isValidUrl(urlInput)) {"
           ,"    alert(\"Please enter a valid URL! (e.g., https://example.com)\");"
           ,"    return;"
           ,"}"

           ,"resultElement.textContent = \"Shortening URL...\";"

            ,"fetch(\"/shorten?url=\" + encodeURIComponent(urlInput), { method:\"POST\" }) "
            ,".then(response => {"
            ,"      if (!response.ok) {"
            ,"          throw new Error(\"Server error\");"
            ,"      }"
            ,"      return response.json();"
            ,"})"
            ,".then(data => {"
            ,"      resultElement.innerHTML = \"Shortened URL:<br><a href='\" + data.short_url + \"'>\" + data.short_url + \"</a>\";"
            ,"})"
            ,".catch(error => {"
            ,"      console.error(\"Error:\", error);"
            ,"      resultElement.textContent = \"Error: Failed to shorten URL\";"
            ,"});"

            ,"}"
            ,"</script>"
            ,"</div>"
            ,"</body>"
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
            
            -- Validate the URL on the server side
            if isValidUrl original
                then do
                    short <- liftIO generateShortURL
                    liftIO $ insertURLMapping conn original short
                    json $ object ["short_url" .= ("http://localhost:3000/" ++ short)]
                else 
                    json $ object ["error" .= ("Invalid URL format." :: String)]


        get "/:short" $ do
            short <- pathParam "short" :: ActionM String
            maybeOriginal <- liftIO $ getOriginalURL conn short

            case maybeOriginal of
                Just original -> redirect (T.pack original)
                Nothing -> text "Short URL not found"