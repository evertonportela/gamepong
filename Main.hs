module Main(main) where

import Graphics.Gloss
    
width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "LPSI - Atv9 Game Haskell Pong" (200, 200) (20, 20)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing
