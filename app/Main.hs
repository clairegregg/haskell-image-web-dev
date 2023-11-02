{-# LANGUAGE OverloadedStrings #-}

module Main(Main.main) where
    
import Data.Text.Lazy
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Html5.Attributes as A
import Network.Wai.Middleware.Static
import Language.Haskell.Interpreter

--exampleDrawing :: [ (Shape, Colour) ]
--exampleDrawing =  [ (polygon [point 0 (-1), point (-0.95) (-0.31), point (-0.59) 0.81, point 0.95 (-0.31), point 0 (-1)], (0, 255, 0)) ]
--exampleDrawing = [(shear (point 0 2) <+> (shear (point 0 2) <+> circle 0.5), (255,0,0))]

exampleDrawingText :: String
exampleDrawingText = "[ (polygon [point 0 (-1), point (-0.95) (-0.31), point (-0.59) 0.81, point 0.95 (-0.31), point 0 (-1)], (0, 255, 0)) ]"

outputFile :: String
outputFile = "static/output.png"

main:: IO ()
main = do 
  result <- liftIO runRenderCode
  liftIO $ putStrLn result
  S.scotty 3000 $ do
    S.middleware $ staticPolicy (noDots >-> addBase "static")
    S.get "/" $ S.html response

runRenderCode :: IO String
runRenderCode = do
  result <- runInterpreter $ do
    setImports ["Prelude", "Shapes", "Render"]
    runStmt ("render \"static/output.png\" defaultWindow " ++ exampleDrawingText)
  case result of
    Left err -> return $ "Error: " ++ show err
    Right _ -> return "Successfully Rendered!" 

response :: Text
response = do 
            R.renderHtml $ do 
              H.head $ H.title "Generating images using eDSL" 
              H.body $ do
                H.h1 "Generated image using Haskell eDSL"
                H.p ("The eDSL code used to generate this image is " >> H.toHtml exampleDrawingText)
                outputImage



outputImage :: H.Html
outputImage = H.img H.! A.src "/output.png" H.! A.alt "Generated image."