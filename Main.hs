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
  old :: [(Float,Float)],
  shape :: Int

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
 shape =  rand
 }


render :: Title -> Picture 
render game = pictures 
 [ 
  walls, draw (old game),component (game),scale (0.2) (0.2) (translate (-1400) (1300) $ color white (text ("Score: " ++ (printf "%.2f" (score game))))),scale (0.2) (0.2) (translate (-1200) (1000) $ color white (text ("shape: " ++ (printf "%.d" (shape game)))))
 ]
draw xs = pictures (helpDraw (xs))

helpDraw [] = []
helpDraw ((x,y):xs) =  [ translate (x) (y) $ color blue $ rectangleSolid 30 30 ] ++ (helpDraw xs)
--call random draw func
component game = pictures (drawrand (shape game) (x,y))
 where
 (x,y) = (componentloc game)

wallH x =  translate (x) (0) $ color  red  $ rectangleSolid 10 600
wallV y =  translate (0) (y) $ color  red  $ rectangleSolid 600 10
walls = pictures [wallH (-300) , wallH (300) , wallV (300) , wallV(-300)]




--random draw

drawrand 1 (x,y) = drawI (x,y)
drawrand 2 (x,y) = drawL (x,y)
drawrand 3 (x,y) = drawO (x,y)



-- draw shapes



drawI (x,y) = drawIhelper 3 (x,y)

drawIhelper 1 (x,y) = [ translate (x) (y*1) $ color blue $ rectangleSolid 30 30 ]

drawIhelper i (x,y) = [ translate (x) (y+(30*(i-1))+(i-1)) $ color blue $ rectangleSolid 30 30 ]++drawIhelper (i-1) (x,y)


drawL (x,y) = drawLhelper 3 (x,y)
drawLhelper 1 (x,y) = [ translate (x) (y*1) $ color blue $ rectangleSolid 30 30 , translate (x+31) (y*1) $ color blue $ rectangleSolid 30 30 ]

drawLhelper i (x,y) = [ translate (x) (y+(30*(i-1))+(i-1)) $ color blue $ rectangleSolid 30 30 ]++drawLhelper (i-1) (x,y)

drawO (x,y) = drawOhelper 2 (x,y)

drawOhelper 0 _ = []

drawOhelper i (x,y) = [ translate (x) (y+(30*(i-1))+(i-1)) $ color blue $ rectangleSolid 30 30,  translate (x+31) (y+(30*(i-1))+(i-1)) $ color blue $ rectangleSolid 30 30]++drawOhelper (i-1) (x,y)



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
shape = if y < ((-fromIntegral width /2)+20) then (unsafePerformIO (getStdRandom (randomR (1, if(mod (length new) 2 ==0) then 3 else 2     )))) else (shape game)
} 
 where 
 (x, y) = (componentloc game)
 z = [(x,y)]
 o = (old game)
 new = o ++ z

rand = unsafePerformIO (getStdRandom (randomR (1,3 )))
main :: IO ()
main = play window background 60 initial render handleKeys update
 where
   
update :: Float -> Title -> Title 
update seconds = redraw . stop . fall seconds
