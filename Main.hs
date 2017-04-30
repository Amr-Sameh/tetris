module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game



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
  old :: [(Float,Float)]
 }


initial :: Title
initial = Game
 {
 componentloc = (0,300),
 componentFall = 20,
 score = 0,
 left = 10,
 right = 10,
 old = []
 }


render :: Title -> Picture 
render game = pictures 
 [ 
  walls, draw (old game),component (game),scale (0.2) (0.2) (translate (-1400) (1300) $ color white (text ("Score: " ++ show (score game))))
 ]
draw xs = pictures (helpDraw (xs))

helpDraw [] = []
helpDraw ((x,y):xs) =  [ translate (x) (y) $ color blue $ rectangleSolid 30 30 ] ++ (helpDraw xs)

component game = pictures [uncurry translate (x, y) $ color blue $  rectangleSolid 30 30  ] 
 where
 (x,y) = (componentloc game)

wallH x =  translate (x) (0) $ color  red  $ rectangleSolid 10 700
wallV y =  translate (0) (y) $ color  red  $ rectangleSolid 700 10
walls = pictures [wallH (-300) , wallH (300) , wallV (300) , wallV(-300)]

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
componentloc = if y < ((-fromIntegral width /2)+20) then (0,300) else (componentloc game)
} 
 where 
 (x, y) = (componentloc game)
 z = [(x,y)]
 o = (old game)
 new = o ++ z
main :: IO ()
main = play window background 60 initial render handleKeys update
 where
   
update :: Float -> Title -> Title 
update seconds = redraw . stop . fall seconds
