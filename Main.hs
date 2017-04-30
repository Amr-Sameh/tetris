module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game



width, height, offset :: Int
width = 700
height = 700
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
 componentloc = (0,350),
 componentFall = 20,
 score = 0,
 left = 10,
 right = 10,
 old = [(50,50),(100,100),(90,90)]
 }


render :: Title -> Picture 
render game = pictures 
 [ 
  walls,fun1 (old game),component (game),scale (0.2) (0.2) (translate (-1700) (1600) $ color white (text ("Score: " ++ show (score game))))
 ]
fun1 xs = pictures (fun (xs))

fun [] = []
fun ((x,y):xs) =  [ translate (x) (y) $ color blue $ rectangleSolid 10 10 ] ++ (fun xs)

component game = pictures [uncurry translate (x, y) $ color blue $  rectangleSolid 10 10 ,
 translate (x+40) (y+0) $ color blue $  rectangleSolid 10 10 ] 
 where
 (x,y) = (componentloc game)

wallH x =  translate (x) (0) $ color  red  $ rectangleSolid 10 700
wallV y =  translate (0) (y) $ color  red  $ rectangleSolid 700 10
walls = pictures [wallH (-350) , wallH (350) , wallV (350) , wallV(-350)]

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
 l = y-(componentFall game)

handleKeys _ game = game

checkLeft x y = x-y > -fromIntegral width /2

checkRight x y = x+y < fromIntegral width /2
checkBottm y r = y-r > -fromIntegral width /2

stop :: Title -> Title
stop game = game{
left = if (checkLeft x 10 && checkBottm y 10) then 10 else 0 ,
right = if (checkRight x 10 && checkBottm y 10) then 10 else 0  ,
componentFall = if (checkBottm y 10 == False) then 0 else 20}
 where
    (x, y) = componentloc game


main :: IO ()
main = play window background 60 initial render handleKeys update
 where
   
update :: Float -> Title -> Title 
update seconds = stop . fall seconds
