import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

import Data.Maybe

import Pages
import Expr



canWidth  = 300
canHeight = 300

points :: Expr -> Double -> (Int,Int) -> [Point]
points e sc (w,h) = zip pixXs $ map (realToPix height sc) $ map (eval e) $ map (pixToReal width sc) pixXs
    where 
         pixXs :: [Double]
         pixXs = [0.0 .. width]
         width = fromIntegral w
         height = fromIntegral h

pixToReal :: Double -> Double -> Double -> Double
pixToReal w sc x = (2*f) * (x / w) - f
    where f = sc * w / 2

realToPix :: Double -> Double -> Double -> Double
realToPix w sc y = w * (-y + f) / (2*f)
    where f = sc * w / 2

parseElem :: Elem -> IO (Maybe Expr)
parseElem e = do
               str <- getValue e
               if isJust str
               then return $ readExpr $ fromJust str
               else return $ Nothing

readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw e c = do
                  expr <- parseElem e
                  if isJust expr
                  then display $ points (fromJust expr) scale size
                  else error "Unkown Expression"
    where display p = render c (stroke (path p))
          scale     = 0.04
          size      = (canHeight,canWidth)

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click $ \_    -> readAndDraw input can
    onEvent input KeyUp $ \code -> when (code==13) $ readAndDraw input can
      -- "Enter" key has code 13

