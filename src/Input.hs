module Input where
-- Define o módulo 'Input', que agrupa funções relacionadas à entrada do usuário.

import Game 
-- Importa o módulo 'Game', que contém definições e funções relacionadas ao jogo.

import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (MouseButton), MouseButton (LeftButton), KeyState (Up))
-- Importa tipos e funções do módulo 'Graphics.Gloss.Interface.IO.Game' para lidar com eventos de entrada, como cliques do mouse.

import Logic (playerTurn)
-- Importa a função 'playerTurn' do módulo 'Logic', que lida com a lógica de mudança de turno dos jogadores.

import System.Random (randomRIO)
-- Importa a função 'randomRIO' do módulo 'System.Random' para gerar números aleatórios dentro de um intervalo.

import Data.List (delete)
-- Importa a função 'delete' do módulo 'Data.List', que remove o primeiro elemento igual a um valor específico de uma lista.

import Control.Monad.State
-- Importa o módulo 'Control.Monad.State', que fornece a monada 'StateT' para manipulação de estado.

-- Converte a posição do mouse em coordenadas de célula.
mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )
-- A função 'mousePosAsCellCoord' converte a posição do mouse (x, y) em coordenadas de célula no tabuleiro.
-- 'floor' arredonda para baixo, e as coordenadas são calculadas com base na largura e altura das células.

randomElement :: Eq a => [a] -> IO (a, [a])
randomElement xs = do
    let len = length xs
    index <- randomRIO (0, len - 1) :: IO Int
    let element = xs !! index
    return (element, delete element xs)
-- A função 'randomElement' seleciona um elemento aleatório de uma lista e retorna o elemento junto com a lista sem esse elemento.
-- 'randomRIO' gera um índice aleatório, e 'delete' remove o elemento selecionado da lista.

pickRandom :: Eq a => [a] -> Int -> IO [a]
pickRandom _ 0 = return []
pickRandom xs amount = do
    (element, rest) <- randomElement xs
    restPicked <- pickRandom rest (amount - 1)
    return (element : restPicked)
-- A função 'pickRandom' seleciona uma quantidade especificada de elementos aleatórios de uma lista.
-- Utiliza 'randomElement' para selecionar elementos e remove os elementos selecionados da lista original.

autoSolve :: StateT Game IO Game
autoSolve = do
    moves <- liftIO $ pickRandom (concat [[(i, j) | i <- [0..boardDimension - 1]] | j <- [0..boardDimension - 1]]) 9
    game <- get
    put $ execState (mapM_ playerTurn moves) game
    get
-- A função 'autoSolve' gera uma sequência de movimentos aleatórios para resolver o jogo automaticamente.
-- 'pickRandom' seleciona todas as posições possíveis no tabuleiro, e 'playerTurn' aplica cada movimento ao estado do jogo.

-- Transforma o estado do jogo com base no evento recebido.
handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> execState (playerTurn (mousePosAsCellCoord mousePos)) game
      GameOver _ -> initialGame
handleEvent _ game = game
-- A função 'handleEvent' transforma o estado do jogo com base no evento recebido.
-- Se o jogo estiver em andamento ('Running') e o botão esquerdo do mouse for liberado, aplica 'playerTurn' na posição do mouse.
-- Se o jogo estiver terminado ('GameOver'), reinicia o jogo com 'initialGame'.
