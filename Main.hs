module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Text.Printf
import System.Random
import System.IO.Unsafe


width, height, offset :: Int
width = 600
height = 600
offset = 100

data Title = Game
 {
  componentloc :: (Float,Float),
  componentFall :: Float,
  score :: Float ,
  left :: Float ,
  right :: Float,
  old :: [((Float,Float),(Int,Int))],
  shape :: Int,
  state :: Int

 }


initial :: Title
initial = Game
 {
 componentloc = (0,300),
 componentFall = 30,
 score = 0,
 left = 30,
 right = 30,
 old = [],
 shape =  rand ,
 state = 0
 }


render :: Title -> Picture 
render game = pictures 
 [ 
  walls, draw (old game),component (game),scale (0.2) (0.2) (translate (-1400) (1300) $ color white (text ("Score: " ++ (printf "%.2f" (score game))))),scale (0.2) (0.2) (translate (-1200) (1000) $ color white (text ("shape: " ++ (printf "%.d" (shape game)))))
 ]
draw xs = pictures (helpDraw (xs))

helpDraw [] = []
helpDraw (((x,y),(s,state)):xs) =  (drawrand s (x,y) state )++ (helpDraw xs)
--call random draw func
component game = pictures (drawrand (shape game) (x,y) (state game))
 where
 (x,y) = (componentloc game)

wallH x =  translate (x) (0) $ color  red  $ rectangleSolid 10 600
wallV y =  translate (0) (y) $ color  red  $ rectangleSolid 600 10
walls = pictures [wallH (-300) , wallH (300) , wallV (300) , wallV(-300)]




--random draw

drawrand 1 (x,y) state = drawI (x,y) state
drawrand 2 (x,y) state = drawL (x,y) state
drawrand 3 (x,y) state = drawO (x,y) 



-- draw shapes



drawI (x,y) 0 = drawIhelper 3 (x,y)
drawI (x,y) 1 = drawIhelper 3 (x,y)
drawI (x,y) 2 = drawIhelper2 3 (x,y)
drawI (x,y) 3 = drawIhelper2 3 (x,y)
drawI (x,y) 4 = drawIhelper 3 (x,y)
drawI (x,y) 5 = drawIhelper 3 (x,y)
drawI (x,y) 6 = drawIhelper2 3 (x,y)
drawI (x,y) 7 = drawIhelper2 3 (x,y)


drawIhelper 1 (x,y) = [ translate (x) (y*1) $ color blue $ rectangleSolid 30 30 ]

drawIhelper i (x,y) = [ translate (x) (y+(30*(i-1))+(i-1)) $ color blue $ rectangleSolid 30 30 ]++drawIhelper (i-1) (x,y)


drawL (x,y) 0 = drawLhelper 3 (x,y)
drawL (x,y) 1 = drawLhelper 3 (x,y)
drawL (x,y) 2 = drawLhelper2 3 (x,y)
drawL (x,y) 3 = drawLhelper2 3 (x,y)
drawL (x,y) 4 = drawLhelper3 3 (x,y)
drawL (x,y) 5 = drawLhelper3 3 (x,y)
drawL (x,y) 6 = drawLhelper4 3 (x,y)
drawL (x,y) 7 = drawLhelper4 3 (x,y)

drawLhelper 1 (x,y) = [ translate (x) (y*1) $ color blue $ rectangleSolid 30 30 , translate (x+31) (y*1) $ color blue $ rectangleSolid 30 30 ]

drawLhelper i (x,y) = [ translate (x) (y+(30*(i-1))+(i-1)) $ color blue $ rectangleSolid 30 30 ]++drawLhelper (i-1) (x,y)

drawO (x,y) = drawOhelper 2 (x,y)

drawOhelper 0 _ = []

drawOhelper i (x,y) = [ translate (x) (y+(30*(i-1))+(i-1)) $ color blue $ rectangleSolid 30 30,  translate (x+31) (y+(30*(i-1))+(i-1)) $ color blue $ rectangleSolid 30 30]++drawOhelper (i-1) (x,y)




--start rotat





 

drawLhelper2 1 (x,y)=[ translate (x) (y*1) $ color blue $ rectangleSolid 30 30, translate (x) (y-31) $ color blue $ rectangleSolid 30 30 ]
drawLhelper2 i (x,y)=[ translate (x+(30*(i-1))+(i-1)) (y) $ color blue $ rectangleSolid 30 30 ]++drawLhelper2 (i-1) (x,y)
 

drawLhelper3 1 (x,y)=[ translate (x) (y*1) $ color blue $ rectangleSolid 30 30, translate (x-31) (y) $ color blue $ rectangleSolid 30 30 ]
drawLhelper3 i (x,y)=[ translate (x) (y-(30*(i-1))-(i-1)) $ color blue $ rectangleSolid 30 30 ]++drawLhelper3 (i-1) (x,y)
 

drawLhelper4 1 (x,y)=[ translate (x) (y*1) $ color blue $ rectangleSolid 30 30, translate (x) (y+31) $ color blue $ rectangleSolid 30 30 ]
drawLhelper4 i (x,y)=[ translate (x-(30*(i-1))-(i-1)) (y) $ color blue $ rectangleSolid 30 30 ]++drawLhelper4 (i-1) (x,y)
 
checkL1 (x,_)=if (x+23) > 300 then 0 else 1
 
checkL2 (_,y)=if (y-23) > -300 then 0 else 1
 
checkL3 (x,_)=if (x-23) > -300 then 0 else 1
 
 
 
 

 

drawIhelper2 1 (x,y) = [ translate (x) (y*1) $ color blue $ rectangleSolid 30 30 ]
drawIhelper2 i (x,y) = [ translate (x+(30*(i-1))+(i-1)) (y) $ color blue $ rectangleSolid 30 30 ]++drawIhelper2 (i-1) (x,y)
 




--end rotate

--end shapes draw





window :: Display
window = InWindow "tetris" (width, height) (offset, offset)

background :: Color
background = black



fall seconds game = game { componentloc = (k,l),score = s}
 where
 (x,y) = componentloc game 
 v = componentFall game
 sc = score game
 k = x + 0
 l = y - v * seconds
 s = sc+0.03
   

handleKeys (EventKey (Char 'f') _ _ _) game = game {componentloc = (0,0)}
handleKeys (EventKey (SpecialKey KeyLeft) _ _ _) game = game {componentloc = (z,l)}
 where 
 (x,y) = componentloc game 
 z = x-(left game)
 l = y+0
handleKeys (EventKey (SpecialKey KeyRight) _ _ _) game = game {componentloc = (z,l)}
 where 
 (x,y) = componentloc game 
 z = x+(right game)
 l = y+0
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game = game {componentloc = (z,l)}
 where 
 (x,y) = componentloc game 
 z = x+0
 l = if (y-(componentFall game))>= ((-fromIntegral width /2)+20) then y-(componentFall game) else  y




--rotate action 
handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game = game {state = mod ((state game)+1) 8}
-- rotate action end
handleKeys _ game = game


checkLeft x y = x-y > -fromIntegral width /2

checkRight x y = x+y < fromIntegral width /2
checkBottm y r = y-r > -fromIntegral width /2

stop :: Title -> Title
stop game = game{
left = if (checkLeft x 20 && checkBottm y 10) then 10 else 0 ,
right = if (checkRight x 20 && checkBottm y 10) then 10 else 0  ,
componentFall = if (checkBottm y 20 == False) then 0 else 20}
 where
    (x, y) = componentloc game

redraw game = game {
old = if y < ((-fromIntegral width /2)+20) then new else (old game),
componentloc = if y < ((-fromIntegral width /2)+20) then (0,300) else (componentloc game),
shape = if y < ((-fromIntegral width /2)+20) then (unsafePerformIO (getStdRandom (randomR (1, if(mod (length new) 2 ==0) then 3 else 2     )))) else (shape game),
state = if y < ((-fromIntegral width /2)+20) then 0 else (state game)
} 
 where 				
 (x, y) = (componentloc game)
 s = (shape game)
 stat = (state game)
 z = [((x,y),(fromIntegral s,fromIntegral stat))]
 o = (old game)
 new = o ++ z

rand = unsafePerformIO (getStdRandom (randomR (1,3 )))
main :: IO ()
main = play window background 60 initial render handleKeys update
 where
   
update :: Float -> Title -> Title 
update seconds = redraw . stop . fall seconds
