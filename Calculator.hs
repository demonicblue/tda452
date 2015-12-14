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
points e sc (w,h) = zip xs ys
    where
         width   = fromIntegral w
         height  = fromIntegral h
         xs      = [0.0 .. width]
         realxs  = map (pixToReal width sc) xs
         realys  = map (eval e) realxs
         ys      = map (realToPix height sc) realys

pixToReal :: Double -> Double -> Double -> Double
pixToReal w sc x = sc * (2*x - w) / 2

realToPix :: Double -> Double -> Double -> Double
realToPix w sc y = -y / sc + w / 2

parseElem :: Elem -> IO (Maybe Expr)
parseElem e = do
               str <- getValue e
               if isJust str
               then return $ readExpr $ fromJust str
               else return $ Nothing

zoomDraw :: Elem -> Elem -> Canvas -> Double -> IO ()
zoomDraw input zoom can step = 
    do
      str <- getValue zoom
      let scale = step + (read (fromJust str)) :: Double
      defaultDraw input zoom can scale

defaultDraw :: Elem -> Elem -> Canvas -> Double -> IO ()
defaultDraw input zoom can scale = do
      set zoom [ prop "value" =: (show scale) ]
      readAndDraw input can scale

readAndDraw :: Elem -> Canvas -> Double -> IO ()
readAndDraw e c sc = do
                  expr <- parseElem e
                  if isJust expr
                  then display $ points (fromJust expr) sc size
                  else error "Unkown Expression"
    where display p = render c (stroke (path p))
          size      = (canHeight,canWidth)

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
    differ  <- mkButton "Differentiate"      -- The differentiate button
    zoomI   <- mkButton "+"                  -- The zoom in button
    zoom    <- mkInput 20 "0"             -- The zoom level input
    zoomO   <- mkButton "-"                  -- The zoom out button
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw,differ,zoomI,zoomO]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    let step  = 0.005
    let scale = 0.04
    -- Interaction
    Just can <- fromElem canvas
    onEvent draw  Click $ \_    -> defaultDraw input zoom can scale
    onEvent zoomI Click $ \_    -> zoomDraw input zoom can (-step)
    onEvent zoomO Click $ \_    -> zoomDraw input zoom can (step)
    onEvent input KeyUp $ \code -> when (code==13) $ defaultDraw input zoom can scale
      -- "Enter" key has code 13

