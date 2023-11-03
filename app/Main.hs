{-# LANGUAGE OverloadedStrings #-}

module Main(Main.main) where
    
import Data.Text.Lazy
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as R
import qualified Text.Blaze.Html5.Attributes as A
import Network.Wai.Middleware.Static
import Language.Haskell.Interpreter

polygonCircleSquare :: String
polygonCircleSquare = "[ (scale  (point 0.25 0.25) <+> (translate (point (-1) (-0.5)) <+> polygon [point 0 (-1), point (-0.95) (-0.31), point (-0.59) 0.81, point 0.95 (-0.31), point 0 (-1)]), (0, 255, 0)), (translate (point 1 0) <+> circle 0.5, (255,0,0)), (shear (point 0.5 0) <+> square 0.25, (0,0,255)) ]"

maskedEllipse :: String
maskedEllipse = "[ (maskedShape (ellipse 0.4 0.8) (square 0.5), (50,100,150)) ]"

cat ::String
cat = "[ (polygon [point 6.09 5.80, point 7.92 7.66, point 7.86 3.89, point 7.97 2.56, point 6.51 0.41, point 3.91 0.20,  point 2.14 2.11, point 2.11 7.78, point 4.77 5.99, point 6.09 5.80], (255, 255, 3))]"

drawings :: [(String,String)]
drawings = [("output1.png",polygonCircleSquare), ("output2.png", maskedEllipse), ("output3.png", cat)]

main:: IO ()
main = do 
  result <- liftIO $ renderDrawings drawings
  liftIO $ putStrLn result
  S.scotty 3000 $ do
    S.middleware $ staticPolicy (noDots >-> addBase "static")
    S.get "/" $ S.html response

renderDrawings :: [(String, String)] -> IO String
renderDrawings [] = return ""
renderDrawings (d:ds) = do 
                          result <- runRenderCode d
                          rest <- renderDrawings ds
                          return (result ++ rest)


runRenderCode :: (String,String) -> IO String
runRenderCode (fileName, dslCode) = do
  result <- runInterpreter $ do
    setImports ["Prelude", "Shapes", "Render"]
    runStmt ("render \"static/"++ fileName ++"\" defaultWindow " ++ dslCode)
  case result of
    Left err -> return $ "Error: " ++ show err
    Right _ -> return "Successfully Rendered!" 

response :: Text
response = do 
            R.renderHtml $ do 
              H.head $ H.title "Generating images using eDSL" 
              H.body $ do
                H.h1 "Generated images using Haskell eDSL"
                outputDivs drawings
  
outputDivs :: [(String, String)] -> H.Html
outputDivs [] = H.p ""
outputDivs (d:ds) = thisImg <> nextImgs
                      where                       
                        thisImg = outputImageDiv d 
                        nextImgs = outputDivs ds

outputImageDiv :: (String, String) -> H.Html
outputImageDiv (fileName, dslCode) = H.div $ do 
                                      H.p "The eDSL code used to generate this image is "
                                      H.code $ H.toHtml dslCode
                                      H.br
                                      H.br
                                      H.img H.! A.src (H.toValue fileName) H.! A.alt "Generated image."
                                      H.hr
