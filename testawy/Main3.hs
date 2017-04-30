module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

drawing :: Picture
drawing = pictures
  [ translate 10 10 $ color paddleColor $ rectangleSolid 10 10
  , translate 10 22 $ color paddleColor $ rectangleSolid 10 10
   , translate 10 34 $ color paddleColor $ rectangleSolid 10 10
  ]
  where
    ballColor = dark red 
    paddleColor = light (light blue) 

main :: IO ()
main = display window background drawing 

