module Main(main) where

import Graphics.Gloss
    
width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "LPSI - Atv9 Game Haskell Pong" (width, height) (offset, offset)

background :: Color
background = black

drawing :: Picture
drawing = pictures [ ball, 
                     walls, 
                     mkPaddle rose 120 (-20), 
                     mkPaddle orange (-120) 40]
          where
            -- The pong balll - A bola do Pong
            ball = translate (-10) 40 $ color ballColor $ circleSolid 10
            ballColor = dark red

            -- The bottom and top walls - As paredes Inferiores e Superiores
            wall :: Float -> Picture
            wall offset = translate 0 offset $ color wallColor $ rectangleSolid 270 10

            wallColor = greyN 0.5
            walls = pictures [wall 150, wall (-150)]
            
            --  Make a paddle of a given border and vertical offset.
            --  Faça uma paleta de uma determinada borda e deslocamento vertical
            mkPaddle :: Color -> Float -> Float -> Picture
            mkPaddle col x y = pictures 
              [ color col $ rectangleSolid 26 86]
            {- drawing :: Picture
drawing = pictures
        [translate (-20) (-100) $ color ballColor $ circleSolid 30,
        translate 30 50 $ color paddleColor $ rectangleSolid 10 50
        ]
        where
          ballColor = dark red
          paddleColor = light (light blue) -}

main :: IO ()
main = display window background drawing
