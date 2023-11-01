{-# LANGUAGE OverloadedStrings #-}

module Main(main) where
    
import Data.Text.Lazy
import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as R
-- I don't actually use any attributes in this example
-- so I commented this next line out, but this is how
-- you get them imported when you need them.
-- import qualified Text.Blaze.Html5.Attributes as A


main:: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html "Hello World!"

{-
  get "/greet/" $ do
      html  "Hello there"
-}

  get "/greet/:name" $ do
      name <- param "name"
      html $ response name

response :: Text -> Text
response n = do R.renderHtml $ do
                  H.h1 ( "Hello " >> H.toHtml n)