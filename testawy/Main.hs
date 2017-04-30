module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 300
height = 300
offset = 100

data A = B
 {
 c :: [(Float,Float)]
 }
init = B 
 {
 c = [(0,0),(30,30),(60,60)]
 }


window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

drawing :: Picture
drawing = pictures (fun ([(0,0),(30,30),(60,60)]))

fun :: [(Float, Float)] -> [Picture]
fun [] = []
fun ((x,y):xs) =  [ translate (x) (y) $ color blue $ rectangleSolid 10 10 ] ++ (fun xs)

main :: IO ()
main = display window background drawing 

