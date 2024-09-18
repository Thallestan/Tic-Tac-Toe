module Logic (verifyGameState, winner, playerTurn) where

import Data.Array
import Data.Foldable (asum)
import Data.Maybe (isNothing)
import Control.Monad.State
import Game

type GameState = State Game
-- Define um tipo 'GameState' como um estado que manipula o tipo 'Game'.

isCoordValid :: (Int, Int) -> Bool
isCoordValid = inRange ((0, 0), (boardDimension - 1, boardDimension - 1))
-- A função 'isCoordValid' verifica se uma coordenada está dentro dos limites do tabuleiro.

switchTurn :: GameState ()
switchTurn = do
    game <- get
    let newPlayer = case gamePlayer game of
                      X -> O
                      O -> X
    put game { gamePlayer = newPlayer }
-- A função 'switchTurn' alterna o jogador atual no estado do jogo.

verifyCells :: [Cell] -> Maybe Player
verifyCells [] = Nothing
verifyCells (Just player:cells) = do
    _ <- foldM (\acc cell -> if cell == Just player then Just player else Nothing) player cells
    return player
verifyCells _ = Nothing

-- A função 'verifyCells' verifica se todas as células em uma lista são ocupadas pelo mesmo jogador,
-- utilizando a propriedade de mônada do tipo Maybe.
-- Retorna 'Just player' se todas as células são iguais, caso contrário, retorna 'Nothing'.

winner :: Board -> Maybe Player
winner board = asum $ map verifyCells $ rows ++ cols ++ diags
    where rows  = [[board ! (i,j) | i <- [0..boardDimension-1]] | j <- [0..boardDimension-1]]
          cols  = [[board ! (j,i) | i <- [0..boardDimension-1]] | j <- [0..boardDimension-1]]
          diags = [[board ! (i,i) | i <- [0..boardDimension-1]]
                  ,[board ! (i,j) | i <- [0..boardDimension-1], let j = boardDimension-1-i ]]
-- A função 'winner' verifica se há um vencedor no tabuleiro.
-- Verifica todas as linhas, colunas e diagonais para ver se todas as células são ocupadas pelo mesmo jogador.

countCells :: Cell -> Board -> Int
countCells cell = length . filter (cell ==) . elems
-- A função 'countCells' conta o número de células no tabuleiro que correspondem a um determinado valor.

verifyGameState :: GameState ()
verifyGameState = do
    game <- get
    let board = gameBoard game
    let newState = case winner board of
                     Just y  -> GameOver $ Just y
                     Nothing -> if countCells Nothing board == 0
                                then GameOver Nothing
                                else Running
    put game { gameState = newState }
-- A função 'verifyGameState' verifica o estado atual do jogo e atualiza o estado do jogo.
-- Se houver um vencedor, define o estado como 'GameOver' com o vencedor.
-- Se todas as células estiverem preenchidas e não houver vencedor, define o estado como 'GameOver' sem vencedor.
-- Caso contrário, mantém o estado como 'Running'.

playerTurn :: (Int, Int) -> GameState ()
playerTurn cellCoord = do
    game <- get
    let board = gameBoard game
    let player = gamePlayer game
    let state = gameState game
    when (state == Running && isCoordValid cellCoord && isNothing (board ! cellCoord)) $ do
        let newBoard = board // [(cellCoord, Just player)]
        put game { gameBoard = newBoard }
        switchTurn
        verifyGameState
-- A função 'playerTurn' realiza a jogada de um jogador em uma coordenada específica.
-- Verifica se o jogo está em andamento, se a coordenada é válida e se a célula está vazia.
-- Atualiza o tabuleiro com a jogada do jogador, alterna o turno e verifica o estado do jogo.
