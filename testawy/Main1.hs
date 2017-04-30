module Main(main, PongGame, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width = 500
height = 500
offset = 20

type Radius = Float
type Radius2 =Float
type Position = (Float, Float)

window :: Display
window = InWindow "Pong" (width, height) (offset , offset)

background :: Color
background = black

fps :: Int
fps = 60

data PongGame = Game
  {
    ballLoc :: (Float, Float),
    ballVel :: (Float, Float),
    player1 :: Float,
    player2 :: Float
    } deriving Show

initialState :: PongGame
initialState = Game
  {
      ballLoc = (0, 0),
      ballVel = (10, -220),
      player1 = 0,
      player2 = 0
      }

render :: PongGame -> Picture
render game = pictures 
  [
    ball, walls,
    mkPaddle rose 200 $ player1 game,
    mkPaddle orange (-200) $ player2 game
  ]

  where
    ball = uncurry translate (ballLoc game) $ color ballcolor $ circleSolid 10
    ballcolor = dark red
    wallV :: Float -> Picture
    wallV offset = translate offset 0 $ color wallcolor $ rectangleSolid 10 480
    wallcolor = greyN 0.5
    wallH offset = translate 0 offset $ color wallhclor $ rectangleSolid 480 10
    wallhclor =  greyN 0.5
    walls = pictures [wallV (245), wallV (-245),wallH (245),wallH (-245)]

    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [
        translate x y $ color col $ rectangleSolid 20 80,
        translate x y $ color paddlecolor $ rectangleSolid 20 80
      ]


    paddlecolor = light (light blue)

moveBall :: Float -> PongGame ->PongGame
moveBall seconds game = game {ballLoc = (x', y')}
  where
    (x,y) = ballLoc game
    (vx,vy) = ballVel game

    x' = x + vx * seconds
    y' = y + vy * seconds

wallCollision1 :: Position -> Radius -> Bool 
wallCollision1 (x, y) radius = topCollision || bottomCollision
  where
          topCollision      = y - radius <= -fromIntegral width / 2 
          bottomCollision = y + radius >=  fromIntegral width / 2
wallCollision2 :: Position -> Radius -> Bool 
wallCollision2 (x, y) radius =  leftCollision || rightCollision
  where
    
    leftCollision      =  x + radius >= fromIntegral height /2
    rightCollision     =  x - radius <= -fromIntegral height /2
    
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (if wallCollision2 (ballLoc game) radius then -vx else vx, if wallCollision1 (ballLoc game) radius then -vy else vy) }
	where
    radius = 10
    (vx, vy) = ballVel game


handleKeys (EventKey (Char 'f') _ _ _) game = game {ballLoc = (0,0)}
handleKeys (EventKey (Char 'w') _ _ _) game = game {player2 = (player2 game)+5}
handleKeys (EventKey (Char 's') _ _ _) game = game {player2 = (player2 game)-5}
handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game = game {player1 = (player1 game)+5}
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game = game {player1 = (player1 game)-5}
handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update
  where
    frame :: Float -> Picture
    frame seconds = render $ moveBall seconds initialState
update :: Float -> PongGame -> PongGame 
update seconds = wallBounce . moveBall seconds
