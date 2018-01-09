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
drawing = pictures [ ball
                   , walls
                   , mkPaddle rose 120 (-20)
                   , mkPaddle orange (-120) 40]
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
              [ translate x y $ color col $ rectangleSolid 26 86
              , translate x y $ color paddleColor $ rectangleSolid 20 80]

            paddleColor = light (light blue)

{- drawing :: Picture
drawing = pictures
        [translate (-20) (-100) $ color ballColor $ circleSolid 30,
        translate 30 50 $ color paddleColor $ rectangleSolid 10 50
        ]
        where
          ballColor = dark red
          paddleColor = light (light blue) -}

-- | Uma estrutura de dados para manter o estado do jogo Pong. Dados que descrevem o Estado do Jogo
data PongGame = Game
        { ballLoc :: (Float, Float) -- ^ Pong ball (x, y) localização.
        , ballVel :: (Float, Float) -- ^ Pong ball (x, y) velocidade.
        , player1 :: Float          -- ^ Altura do Bastao do Jogador Esquerdo.
                                    -- Zero é o meio da tela.
        , player2 :: Float          -- ^ Altura do Bastao do Jogador Direito.
        } deriving Show

-- | Desenha um estado de jogo do pong (convertendo-o em uma imagem).
render :: PongGame      -- ^ O estado do jogo para renderizar
       -> Picture       -- ^ Uma imagem deste estado do jogo.
render game = pictures 
            [ ball
            , walls
            , mkPaddle rose 120 $ player1 game
            , mkPaddle orange (-120) $ player2 game]
    where
      -- A bola Pong
      ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
      ballColor = dark red

      -- As paredes inferiores e superiores
      wall :: Float -> Picture
      wall offset = translate 0 offset $ color wallColor $ rectangleSolid 270 10

      wallColor = greyN 0.5
      walls = pictures [ wall 150, wall (-150)]

      --  Make a paddle of a given border and vertical offset.
      --  Faça uma paleta de uma determinada borda e deslocamento vertical
      mkPaddle :: Color -> Float -> Float -> Picture
      mkPaddle col x y = pictures 
        [ translate x y $ color col $ rectangleSolid 26 86
        , translate x y $ color paddleColor $ rectangleSolid 20 80]

      paddleColor = light (light blue)

-- | Update the ball position using current velocity.
-- | Atualizar posicao da bola usando a velocidade atual.
moveBall :: Float    -- ^ O numero de segundos desde a ultima atualizacao.
         -> PongGame -- ^ o estado inicial do jogo.
         -> PongGame -- ^ Um novo estado do jogo com atualizao da posicao da bola.
moveBall seconds game = game { ballLoc = (x', y') }
   where
     -- Local e velocidades antigas
     (x, y) = ballLoc game
     (vx, vy) = ballVel game

     -- Nova localizacoes
     x' = x + vx * seconds
     y' = y + vy * seconds


-- | O estado  inicial do Jogo
initialState :: PongGame
initialState = Game
         { ballLoc = (-10, 30)
         , ballVel = (1, -3)
         , player1 = 40
         , player2 = -80 }

main :: IO ()
main = animate window background frame
  where
    frame :: Float -> Picture
    frame seconds = render $ moveBall seconds initialState
{- main :: IO ()
main = display window background drawing -}
