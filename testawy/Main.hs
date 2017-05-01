module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 600
height = 600
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
drawing = pictures (drawO (170,230))

drawI (x,y) = drawIhelper 5 (x,y)

drawIhelper 1 (x,y) = [ translate (x) (y*1) $ color blue $ rectangleSolid 10 10 ]

drawIhelper i (x,y) = [ translate (x) (y+(10*(i-1))+(i-1)) $ color blue $ rectangleSolid 10 10 ]++drawIhelper (i-1) (x,y)


drawL (x,y) = drawLhelper 3 (x,y)
drawLhelper 1 (x,y) = [ translate (x) (y*1) $ color blue $ rectangleSolid 10 10 , translate (x+11) (y*1) $ color blue $ rectangleSolid 10 10 ]

drawLhelper i (x,y) = [ translate (x) (y+(10*(i-1))+(i-1)) $ color blue $ rectangleSolid 10 10 ]++drawLhelper (i-1) (x,y)

drawO (x,y) = drawOhelper 2 (x,y)

drawOhelper 0 _ = []

drawOhelper i (x,y) = [ translate (x) (y+(10*(i-1))+(i-1)) $ color blue $ rectangleSolid 10 10,  translate (x+11) (y+(10*(i-1))+(i-1)) $ color blue $ rectangleSolid 10 10]++drawOhelper (i-1) (x,y)


fun :: [(Float, Float)] -> [Picture]
fun [] = []
fun ((x,y):xs) =  [ translate (x) (y) $ color blue $ rectangleSolid 10 10 ] ++ (fun xs)

main :: IO ()
main = display window background drawing 

