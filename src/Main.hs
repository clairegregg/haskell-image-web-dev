{-# LANGUAGE OverloadedStrings #-}

module Main(Main.main) where
    
import Data.Text.Lazy
import qualified Web.Scotty as S
import Text.Blaze.Html5 
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Html5.Attributes as A
import Network.Wai.Middleware.Static


main:: IO ()
main = S.scotty 3000 $ do
  S.middleware $ staticPolicy (noDots >-> addBase "static")
  S.get "/" $ S.html response

response :: Text
response = R.renderHtml outputImage

outputImage :: Html
outputImage = img ! A.src "output.png" ! A.alt "Generated image."