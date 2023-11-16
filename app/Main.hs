{-# LANGUAGE OverloadedStrings #-}

module Main(Main.main) where
    
import Data.Text.Lazy
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Html5.Attributes as A
import Network.Wai.Middleware.Static
import Language.Haskell.Interpreter

--------------
-- DRAWINGS --
--------------

polygonCircleSquare :: String
polygonCircleSquare = "[ (scale  (point 0.25 0.25) <+> (translate (point (-1) (-0.5)) <+> polygon [point 0 (-1), point (-0.95) (-0.31), point (-0.59) 0.81, point 0.95 (-0.31), point 0 (-1)]), (0, 255, 0)), (translate (point 1 0) <+> circle 0.5, (255,0,0)), (shear (point 0.5 0) <+> square 0.25, (0,0,255)) ]"

maskedEllipse :: String
maskedEllipse = "[ (maskedShape (ellipse 0.4 0.8) (square 0.5), (50,100,150)) ]"

cat :: String
cat = "[ (translate (point 0 (-0.4)) <+> rectangle 0.2 0.05, (201,41,94)), (translate (point (-0.4) 0.2) <+> (scale (point 0.5 0.5) <+> (maskedShape (translate (point (-0.4) 0) <+> circle 0.5) (translate (point 0.4 0) <+> circle 0.5))), (1, 1, 1)) ,(translate (point (-0.4) 0.2) <+> ellipse 0.25 0.15, (3,82,3)), (translate (point 0.4 0.2) <+> (scale (point 0.5 0.5) <+> (maskedShape (translate (point (-0.4) 0) <+> circle 0.5) (translate (point 0.4 0) <+> circle 0.5))), (1, 1, 1)) ,(translate (point 0.4 0.2) <+> ellipse 0.25 0.15, (3,82,3)), ((translate (point (-0.8) 1.1) <+> (scale (point 0.5 0.5) <+> ( rotate (-3.5) <+> polygon [point (-1) 0, point 0 1.5, point 1 0]))), (82, 45, 3)), (translate (point 0.8 1.1) <+> (scale (point 0.5 0.5) <+> ( rotate 3.5 <+> polygon [point (-1) 0, point 0 1.5, point 1 0])), (82, 45, 3)), (circle 1.2, (97, 54, 4)) ]"

-- All drawings which will be rendered
drawings :: [(String,String)]
drawings = [("output1.png", polygonCircleSquare), ("output2.png", maskedEllipse), ("output3.png", cat)]

-- Main renders all drawings (outputting any render errors to command line).
-- Then it sets up the Scotty server, serving static files from the `static`directory.
main:: IO ()
main = do 
  result <- liftIO $ renderDrawings drawings
  liftIO $ putStrLn result
  S.scotty 3000 $ do
    S.middleware $ staticPolicy (noDots >-> addBase "static")
    S.get "/" $ S.html response

-- Renders all drawings which are inputted (as code and file names)
renderDrawings :: [(String, String)] -> IO String
renderDrawings [] = return ""
renderDrawings (d:ds) = do 
                          result <- runRenderCode d
                          rest <- renderDrawings ds
                          return (result ++ rest)

-- Renders an images given its code (interpreted using `hint`) and a file name.
-- If an error in the render, an error is returned in the IO String
runRenderCode :: (String,String) -> IO String
runRenderCode (fileName, dslCode) = do
  result <- runInterpreter $ do
    setImports ["Prelude", "Shapes", "Render"]
    runStmt ("render \"static/"++ fileName ++"\" defaultWindow " ++ dslCode)
  case result of
    Left err -> return $ "Error: " ++ show err
    Right _ -> return "Successfully Rendered!" 

-- Full HTML output for website
response :: Text
response = do 
            R.renderHtml $ do 
              H.head $ H.title "Generating images using eDSL" 
              H.body $ do
                H.h1 "Generated images using Haskell eDSL"
                outputDivs drawings
  
-- Div containing all generated image divs
outputDivs :: [(String, String)] -> H.Html
outputDivs [] = H.p ""
outputDivs (d:ds) = thisImg <> nextImgs
                      where                       
                        thisImg = outputImageDiv d 
                        nextImgs = outputDivs ds

-- Div to contain an image and its code
outputImageDiv :: (String, String) -> H.Html
outputImageDiv (fileName, dslCode) = H.div $ do 
                                      H.p "The eDSL code used to generate this image is "
                                      H.code $ H.toHtml dslCode
                                      H.br
                                      H.br
                                      H.img H.! A.src (H.toValue fileName) H.! A.alt "Generated image."
                                      H.hr
