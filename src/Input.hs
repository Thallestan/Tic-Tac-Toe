module Input where

import Game
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (MouseButton), MouseButton (LeftButton), KeyState (Up))
import Logic (playerTurn)
import System.Random (randomRIO)
import Data.List (delete)
import Control.Monad.State
import Data.Array

-- Converte a posição do mouse em coordenadas de célula.
mousePosAsCellCoord :: (Float, Float) -> Int -> (Int, Int)
mousePosAsCellCoord (x, y) depth = (
      floor (x / (cellWidth / (3 ^ depth))) + 1
    , floor (y / (cellHeight / (3 ^ depth))) + 1
    )
-- A função 'mousePosAsCellCoord' converte a posição do mouse (x, y) em coordenadas de célula no tabuleiro.
-- 'floor' arredonda para baixo, e as coordenadas são calculadas com base na largura e altura das células.

randomElement :: Eq a => [a] -> IO (a, [a])
randomElement xs = do
    let len = length xs
    i <- randomRIO (0, len - 1) :: IO Int
    let element = xs !! i
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
    game <- get
    let depth = treeDepth (gameBoard game) + 1
    let movesArr = range ((1, 1), (3 ^ depth, 3 ^ depth))
    moves <- liftIO $ pickRandom movesArr (length movesArr)
    put $ execState (mapM_ playerTurn moves) game
    get
-- A função 'autoSolve' gera uma sequência de movimentos aleatórios para resolver o jogo automaticamente.
-- 'pickRandom' seleciona todas as posições possíveis no tabuleiro, e 'playerTurn' aplica cada movimento ao estado do jogo.

-- Transforma o estado do jogo com base no evento recebido.
handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Up _ (mx, my)) game
    | not (x > 0 && x <= fromIntegral screenWidth && y > 0 && y <= fromIntegral screenHeight) = game
    | otherwise = case gameState game of
        Running -> execState (playerTurn (mousePosAsCellCoord (x, y) $ treeDepth $ gameBoard game)) game
        GameOver _ -> game
    where x = mx + (fromIntegral screenWidth / 2)
          y = my + (fromIntegral screenHeight / 2)
handleEvent _ game = game
-- A função 'handleEvent' transforma o estado do jogo com base no evento recebido.
-- Se o jogo estiver em andamento ('Running') e o botão esquerdo do mouse for liberado, aplica 'playerTurn' na posição do mouse.
-- Se o jogo estiver terminado ('GameOver'), reinicia o jogo com 'initialGame'.

