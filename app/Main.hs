{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- | This module (Main.hs) sets up the web server using the Scotty framework.
-- | It defines the routes for the URL shortener application, handles HTTP requests,
-- | interacts with the SQLite database, and manages an in-memory URL store.
-- | It also serves the HTML pages for the user interface.

-- Import necessary modules
import Prelude hiding (null) -- | Standard Haskell prelude, hiding 'null' to avoid conflicts.
import Lib -- | Custom module containing core URL shortening logic and database interaction.
import Web.Scotty -- | Web framework for creating web applications.
import qualified Data.Text.Lazy as T -- | Lazy Text for efficient text manipulation.
import Data.Aeson -- | JSON encoding and decoding.
import Database.SQLite.Simple -- | SQLite database interaction.
import Data.Text.Lazy (pack, unpack, null) -- | Lazy Text for efficient text manipulation.
import Control.Concurrent.STM (atomically, newTVar, readTVar) -- | Software Transactional Memory for concurrent operations.
import Control.Exception (SomeException) -- | Exception handling.
import qualified Data.Map as Map -- | Maps for in-memory URL storage.

-- Serve the HTML page that displays the shortened URLs and allows the user to shorten a new URL
-- Fetches URL mappings from the SQLite database and renders an HTML page
serveHTML :: Connection -> ActionM ()
serveHTML conn = do
    -- Fetch all URL mappings from the database
    mappings <- liftIO $ getAllMappings conn
    -- Construct HTML for the list of previously shortened URLs
    let previousUrls = T.concat
            [ "<p>Shortened URLs:</p>"
            , "<div class='url-list'>"
            , T.concat
                [ "<div class='url-item'><a href=\"http://localhost:3000/" <> T.pack short <> "\" class='short-url' target='_blank'>" <> T.pack short <> "</a> <span class='arrow'>→</span> <span class='original-url'>" <> T.pack original <> "</span></div>"
                | URLMapping original short <- mappings
                ]
            , "</div>"
            ]

    -- Render the complete HTML page
    html $ T.concat
        [ "<html>"
        , "<head>"
        , "<title>URL Shortener</title>"
        , "<style>"
        , "* { margin: 0; padding: 0; box-sizing: border-box; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }"
        , "body { min-height: 100vh; background: linear-gradient(135deg, #f0f4ff 0%, #e6eeff 100%); display: flex; justify-content: center; align-items: center; padding: 20px; }"
        , ".container { background: white; padding: 2rem; border-radius: 12px; box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1); width: 100%; max-width: 600px; }"
        , "h1 { color: #2d3748; text-align: center; margin-bottom: 1.5rem; font-size: 2rem; }"
        , ".input-section { display: flex; gap: 1rem; margin-bottom: 1.5rem; }"
        , ".url-list { margin-top: 1rem; border-top: 2px solid #e2e8f0; padding-top: 1rem; }"
        , ".url-item { padding: 0.5rem 0; display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #e2e8f0; }"
        , ".short-url { color: #4299e1; text-decoration: none; font-weight: bold; }"
        , ".short-url:hover { text-decoration: underline; }"
        , ".original-url { color: #4a5568; }"
        , ".arrow { margin: 0 10px; font-weight:bold; }"
        , "input { flex: 1; padding:.75rem ; border: 2px solid #e2e8f0; border-radius: 8px; font-size:1rem; transition:.3s; }"
        , "input:focus { outline:none; border-color:#4299e1; box-shadow:0 0 0 3px rgba(66,153,225,0.15); }"
        , "button { padding:.75rem 1.5rem; background-color:#4299e1; color:white; border-radius:8px; border:none; cursor:pointer; font-size:1rem; transition:.3s; }"
        , "button:hover { background-color:#3182ce; }"
        , "#result { background-color:#f7fafc; border-radius:8px; padding:.75rem; margin-top:.5rem; }"
        , "@media (max-width:480px) { .input-section { flex-direction: column; } button { width:100%; margin-top:.5rem; } }"
        , "</style>"
        , "</head>"
        , "<body>"
        , "<div class='container'>"
        , "<h1>URL Shortener</h1>"
        , "<div class='input-section'>"
        , "<input type='text' id='urlInput' placeholder='Enter your URL here' aria-label='URL input'/>"
        , "<button onclick='shortenUrl()'>Shorten URL</button>"
        , "</div>"
        , "<div id='result' class='output-box'>"
        , previousUrls
        , "</div>"
        , "<script>"
        , "function shortenUrl() {"
        , "const urlInput = document.getElementById(\"urlInput\").value.trim();"
        , "const resultElement = document.getElementById(\"result\");"
        , "function isValidUrl(string) { try { new URL(string); return true; } catch (_) { return false; } }"
        , "if (!urlInput) { alert(\"Please enter a URL!\"); return; }"
        , "if (!isValidUrl(urlInput)) { alert(\"Please enter a valid URL! (e.g., https://example.com)\"); return; }"
        , "resultElement.textContent = \"Shortening URL...\";"
        , "fetch(\"/shorten?url=\" + encodeURIComponent(urlInput), { method:\"POST\" }) "
        , ".then(response => { if (!response.ok) { throw new Error(\"Server error\"); } return response.json(); })"
        , ".then(data => { resultElement.innerHTML = \"Shortened URL:<br><a href='\" + data.short_url + \"' target='_blank'>>\" + data.short_url + \"</a>\"; })"
        , ".catch(error => { console.error(\"Error:\", error); resultElement.textContent = \"Error: Failed to shorten URL\"; });"
        , "}"
        , "</script>"
        , "</div>"
        , "</body>"
        , "</html>"
        ]


passwordForm :: String -> String
passwordForm code =
  "<html>" ++
  "<head>" ++
  "<title>Enter Password</title>" ++
  "<style>" ++
  "* { margin: 0; padding: 0; box-sizing: border-box; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }" ++
  "body { min-height: 100vh; background: linear-gradient(135deg, #f0f4ff 0%, #e6eeff 100%); display: flex; justify-content: center; align-items: center; padding: 20px; }" ++
  ".container { background: white; padding: 2rem; border-radius: 12px; box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1); width: 100%; max-width: 600px; }" ++
  "h1 { color: #2d3748; text-align: center; margin-bottom: 1.5rem; font-size: 2rem; }" ++
  ".input-section { display: flex; gap: 1rem; margin-bottom: 1.5rem; }" ++
  ".url-list { margin-top: 1rem; border-top: 2px solid #e2e8f0; padding-top: 1rem; }" ++
  ".url-item { padding: 0.5rem 0; display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #e2e8f0; }" ++
  ".short-url { color: #4299e1; text-decoration: none; font-weight: bold; }" ++
  ".short-url:hover { text-decoration: underline; }" ++
  ".original-url { color: #4a5568; }" ++
  ".arrow { margin: 0 10px; font-weight:bold; }" ++
  "input { flex: 1; padding:.75rem ; border: 2px solid #e2e8f0; border-radius: 8px; font-size:1rem; transition:.3s; }" ++
  "input:focus { outline:none; border-color:#4299e1; box-shadow:0 0 0 3px rgba(66,153,225,0.15); }" ++
  "button { padding:.75rem 1.5rem; background-color:#4299e1; color:white; border-radius:8px; border:none; cursor:pointer; font-size:1rem; transition:.3s; }" ++
  "button:hover { background-color:#3182ce; }" ++
  "#result { background-color:#f7fafc; border-radius:8px; padding:.75rem; margin-top:.5rem; }" ++
  "@media (max-width:480px) { .input-section { flex-direction: column; } button { width:100%; margin-top:.5rem; } }" ++
  "</style>" ++
  "</head>" ++
  "<body>" ++
  "<div class='container'>" ++
  "<h1>Enter Password</h1>" ++
  "<form method='post' action='/verify/" ++ code ++ "'>" ++
  "<div class='input-section'>" ++
  "<input type='password' name='password' placeholder='Enter password' aria-label='Password input'/>" ++
  "<button type='submit'>Submit</button>" ++
  "</div>" ++
  "</form>" ++
  "</div>" ++
  "</body>" ++
  "</html>"

-- The main function to set up the server and handle routing
main :: IO ()
main = do
    -- Open the SQLite database connection
    conn <- open "urls.db"
    -- Create the 'urls' table if it doesn't exist
    createTable conn
    -- Initialize the in-memory URL store
    storeVar <- atomically $ newTVar Map.empty
    -- Start the Scotty web server on port 3000
    scotty 3000 $ do
        -- Route for the main HTML page
        get "/" $ serveHTML conn
        -- Route for shortening URLs (SQLite database)
        post "/shorten" $ do
            -- Get the URL from the query parameters
            original <- queryParam "url" :: ActionM String
            -- Validate the URL
            if isValidUrl original
                then do
                    -- Generate a short URL
                    short <- liftIO generateShortURL
                    -- Insert the URL mapping into the database
                    liftIO $ insertURLMapping conn original short
                    -- Return the shortened URL as JSON
                    json $ object ["short_url" .= ("http://localhost:3000/" ++ short)]
                else
                    -- Return an error message as JSON
                    json $ object ["error" .= ("Invalid URL format." :: String)]

        -- Route for shortening URLs (in-memory store)
        post "/shorten/memory" $ do
            -- Get the URL from the form parameters
            longUrl <- formParam "url"
            -- Get the password from the form parameters, or an empty string if not provided
            pass <- formParam "password" `catch` (\(_ :: SomeException) -> return "")
            -- Generate a short code and store the URL entry in the in-memory store
            shortCode <- liftIO $ generateShortCode storeVar (unpack longUrl) (if null pass then Nothing else Just (unpack pass))
            -- Return the shortened URL as JSON
            json $ object ["short_url" .= ("http://localhost:3000/" ++ shortCode)]

        -- Route for redirecting from a short URL to the original URL
        get "/:short" $ do
            -- Get the short URL from the path parameters
            short <- pathParam "short" :: ActionM String
            -- Try to get the original URL from the database
            maybeOriginal <- liftIO $ getOriginalURL conn short
            case maybeOriginal of
                -- If found in the database, redirect to the original URL
                Just original -> redirect (T.pack original)
                -- If not found in the database, try to get it from the in-memory store
                Nothing -> do
                    maybeOriginalInMemory <- liftIO $ atomically $ readTVar storeVar >>= return . Map.lookup short
                    case maybeOriginalInMemory of
                        -- If found in the in-memory store
                        Just entry -> case password entry of
                            -- If no password is required, redirect to the original URL
                            Nothing -> redirect (pack $ originalUrl entry)
                            -- If a password is required, serve the password form
                            Just _ -> html $ pack $ passwordForm short
                        -- If not found in the in-memory store, return an error message
                        Nothing -> text "Short URL not found"

        -- Route for verifying the password and redirecting to the original URL
        post "/verify/:short" $ do
            -- Get the short URL from the path parameters
            short <- pathParam "short"
            -- Get the password from the form parameters
            inputPass <- formParam "password"
            -- Get the in-memory store
            store <- liftIO $ atomically $ readTVar storeVar
            case Map.lookup short store of
                -- If the short URL is not found, return an error message
                Nothing -> html "Link not found!"
                -- If the short URL is found
                Just entry -> case password entry of
                    -- If no password is required, redirect to the original URL
                    Nothing -> redirect (pack $ originalUrl entry)
                    -- If a password is required
                    Just storedPass ->
                        -- If the input password matches the stored password, redirect to the original URL
                        if inputPass == storedPass
                            then redirect (pack $ originalUrl entry)
                            -- If the input password is incorrect, return an error message
                            else html "Incorrect password!"