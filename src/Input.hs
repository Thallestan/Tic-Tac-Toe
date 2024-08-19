module Input where
import Game (Game (gameState), State (Running, GameOver), screenHeight, cellHeight, screenWidth, cellWidth, initialGame, boardDimension)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (MouseButton), MouseButton (LeftButton), KeyState (Up))
import Logic (playerTurn)
import System.Random (randomRIO)
import Data.List (delete)

-- Converte a posição do mouse em coordenadas de célula.
mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )
randomElement :: Eq a => [a] -> IO (a, [a])
randomElement xs = do
    let len = length xs
    index <- randomRIO (0, len - 1) :: IO Int
    let element = xs !! index
    return (element, delete element xs)

pickRandom :: Eq a => [a] -> Int -> IO [a]
pickRandom _ 0 = return []
pickRandom xs amount = do
    (element, rest) <- randomElement xs
    restPicked <- pickRandom rest (amount - 1)
    return (element : restPicked)

autoSolve :: Game -> IO Game
autoSolve game = do
    moves <- pickRandom (concat [[(i, j) | i <- [0..boardDimension - 1]] | j <- [0..boardDimension - 1]]) 9 
    return (foldl playerTurn game moves)

--Transforma o estado do jogo com base no evento recebido.
handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> playerTurn game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame
handleEvent _ game = game
