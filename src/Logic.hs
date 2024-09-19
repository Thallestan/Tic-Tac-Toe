module Logic (verifyGameState, winner, playerTurn) where

import Data.Maybe (isNothing)
import Control.Monad.State
import Game
import Data.Vector (fromList)
import Data.Matrix (getRow, getCol, getDiag, getElem, setElem)

type GameState = State Game
-- Define um tipo 'GameState' como um estado que manipula o tipo 'Game'.

switchTurn :: GameState ()
switchTurn = do
    game <- get
    let newPlayer = case gamePlayer game of
                      X -> O
                      O -> X
    put game { gamePlayer = newPlayer }
-- A função 'switchTurn' alterna o jogador atual no estado do jogo.

winner :: Board -> Maybe Player
winner (Leaf board)
    | any (all (\c -> c == Just X)) (rows ++ cols ++ diags) = Just X
    | any (all (\c -> c == Just O)) (rows ++ cols ++ diags) = Just O
    | otherwise = Nothing
    where rows  = [getRow i board | i <- [1..boardDimension]]
          cols  = [getCol i board | i <- [1..boardDimension]]
          diags = getDiag board : [fromList [getElem i (boardDimension - i + 1) board | i <- [1..boardDimension]]]
winner (Node n)
    | any (all (\c -> c == Just X)) (rows ++ cols ++ diags) = Just X
    | any (all (\c -> c == Just O)) (rows ++ cols ++ diags) = Just O
    | otherwise = Nothing
    where rows = [getRow i board | i <- [1..boardDimension]]
          cols = [getCol i board | i <- [1..boardDimension]]
          diags = getDiag board : [fromList [getElem i (boardDimension - i + 1) board | i <- [1..boardDimension]]]
          board = fmap winner n
-- A função 'winner' verifica se há um vencedor no tabuleiro.
-- Verifica todas as linhas, colunas e diagonais para ver se todas as células são ocupadas pelo mesmo jogador.

boardAll :: (Cell -> Bool) -> Board -> Bool
boardAll f (Leaf board) = all f board
boardAll _ (Node _) = False

verifyGameState :: GameState ()
verifyGameState = do
    game <- get
    let board = gameBoard game
    let newState = case winner board of
                     Just y  -> GameOver $ Just y
                     Nothing -> if boardAll isNothing board
                                then GameOver Nothing
                                else Running
    put game { gameState = newState }
-- A função 'verifyGameState' verifica o estado atual do jogo e atualiza o estado do jogo.
-- Se houver um vencedor, define o estado como 'GameOver' com o vencedor.
-- Se todas as células estiverem preenchidas e não houver vencedor, define o estado como 'GameOver' sem vencedor.
-- Caso contrário, mantém o estado como 'Running'.

ceilDiv :: Int -> Int -> Int
ceilDiv a b = (a + b - 1) `div` b

isCoordValid :: (Int, Int) -> Board -> Bool
isCoordValid (i, j) (Leaf board) = isNothing (winner (Leaf board)) && isNothing (getElem i j board)
isCoordValid (i, j) (Node board) = isNothing (winner (Node board)) && isCoordValid (ei, ej) (getElem ni nj board)
    where (ni, nj) = (ceilDiv i (3 ^ depth), ceilDiv j (3 ^ depth))
          (ei, ej) = (i - ((3 ^ depth) * (ni - 1)), j - ((3 ^ depth) * (nj - 1)))
          depth = treeDepth (Node board)

putBoard :: Maybe Player -> (Int, Int) -> Board -> Board
putBoard p pos (Leaf board) = Leaf (setElem p pos board)
putBoard p (i, j) (Node board) = Node $ setElem (putBoard p (ei, ej) (getElem ni nj board)) (ni, nj) board
    where (ni, nj) = (ceilDiv i (3 ^ depth), ceilDiv j (3 ^ depth))
          (ei, ej) = (i - ((3 ^ depth) * (ni - 1)), j - ((3 ^ depth) * (nj - 1)))
          depth = treeDepth (Node board)

playerTurn :: (Int, Int) -> GameState ()
playerTurn cellCoord = do
    game <- get
    let board = gameBoard game
    let player = gamePlayer game
    let gState = gameState game
    when (gState == Running && isCoordValid cellCoord board) $ do
        let newBoard = putBoard (Just player) cellCoord board
        put game { gameBoard = newBoard }
        switchTurn
        verifyGameState
-- A função 'playerTurn' realiza a jogada de um jogador em uma coordenada específica.
-- Verifica se o jogo está em andamento, se a coordenada é válida e se a célula está vazia.
-- Atualiza o tabuleiro com a jogada do jogador, alterna o turno e verifica o estado do jogo.
