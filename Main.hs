module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
    
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
    --  Faça uma paleta de uma determinada borda e deslocamento vertical.
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
      -- Local e velocidades antigas.
      (x, y) = ballLoc game
      (vx, vy) = ballVel game

      -- Nova localizacoes.
      x' = x + vx * seconds
      y' = y + vy * seconds
  
-- | Executar uma janela de tempo finito.
simulate :: Display        -- ^ Como exibir o jogo.
           -> Color          -- ^ Cor de Fundo.
           -> Int            -- ^ Numero de etapas de simulacoes a serem tomadas por segundo.
           -> a              -- ^ O estado de inicio do jogo.
           -> (a -> Picture) -- ^ Um funcao para renderizar o jogo para uma imagem.
           -> (ViewPort -> Float -> a -> a) -- ^ Uma funcao a respeito de cada etapa do jogo
           -> IO ()
-- | O estado  inicial do Jogo
initialState :: PongGame
initialState = Game
           { ballLoc = (-10, 30)
           , ballVel = (1, -3)
           , player1 = 40
           , player2 = -80 }
  
-- | Numero de quadros a mostrar por segundo
fps :: Int
fps = 60
  
-- | Atualizacao do Jogo movendo a bola
-- Ignorando o argumento ViewPort
update :: ViewPort -> Float -> PongGame -> PongGame
update _ seconds = wallBounce . moveBall seconds

-- | Detectar uma colisao com um bastao.
paddleBounce :: PongGame -> PongGame

-- | Detectar uma coliaso com uma das paredes laterais.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) radius
          then
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy

type Radius = Float 
type Position = (Float, Float)

-- | Dada a posicao e o raio da bola, retorna se ocorreu uma colisao.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral width / 2 
    bottomCollision = y + radius >=  fromIntegral width / 2

-- | Roda o jogo em uma janela
play :: Display -- ^ Janela para desenhar o jogo
     -> Color   -- ^ Cor de fundo
     -> Int     -- ^ Numero de etapas de simulacao por segundo de tempo real
     -> a       -- ^ O estado inicial do jogo
     -> (a -> Picture)       -- ^ Uma funcao para tornar o mundo uma imagem
     -> (Event -> a -> a)    -- ^ Uma funcao para lidar com eventos de entrada.
     -> (Float -> a -> a)    -- ^ Uma funcao para a etapa de iteracao.
     -> IO ()

-- | Respondendo eventos-chave
handleKeys :: Event -> PongGame -> PongGame

-- Para uma tecla 'S', reponha a bola para o centro
handleKeys (EventKey (Char 's') _ _ _) game =
  game { ballLoc = (0, 0) }

-- Nao fazer nada para todos os eventos.
handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update
