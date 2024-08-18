module Logic (verifyGameState
  , winner
  , playerTurn
  ) where

{-
Import de bibliotecas padrões Haskell
    Data.Array para manipulação de arrays;
    função 'asum' da biblioteca Data.Foldable;
    função 'isNothing' da biblioteca Data.Maybe;
    interface gráfica do Gloss.
-}

import Data.Array

import Data.Foldable (asum)

import Data.Maybe (isNothing)

-- Importa módulos personalizados do jogo.
import Game


-- Verifica se a coordenada está dentro do intervalo permitido.
isCoordValid :: (Int, Int) -> Bool
isCoordValid = inRange ((0, 0), (boardDimension - 1, boardDimension - 1))


-- Alterna o jogador atual no jogo.
switchTurn :: Game -> Game
switchTurn game =
    case gamePlayer game of
      X -> game { gamePlayer = O }
      O -> game { gamePlayer = X }

-- Verifica se todas as células em uma lista são iguais e não vazias.
verifyCells :: [Cell] -> Maybe Player
verifyCells [] = Nothing
verifyCells (Just player:cells) =
    foldr (\cell acc -> if cell == Just player then acc else Nothing) (Just player) cells
verifyCells _ = Nothing


{- 
Determina o vencedor do jogo verificando linhas, colunas e diagonais.
Uso da função sum para combinar o resultado do map e validar se existe um vencedor em alguma linha, coluna ou diagonal.
Retorna o primeiro 'Just Player' que encontrar
-}
winner :: Board -> Maybe Player
winner board = asum $ map verifyCells $ rows ++ cols ++ diags
    where rows  = [[board ! (i,j) | i <- [0..boardDimension-1]] | j <- [0..boardDimension-1]]
          cols  = [[board ! (j,i) | i <- [0..boardDimension-1]] | j <- [0..boardDimension-1]]
          diags = [[board ! (i,i) | i <- [0..boardDimension-1]]
                  ,[board ! (i,j) | i <- [0..boardDimension-1], let j = boardDimension-1-i ]]

-- Conta o número de células específicas no tabuleiro.
countCells :: Cell -> Board -> Int
countCells cell = length . filter (cell ==) . elems

-- Verifica se o jogo terminou e atualiza o estado do jogo.
verifyGameState :: Game -> Game
verifyGameState game
    | Just y <- winner board = game { gameState = GameOver $ Just y }
    | countCells Nothing board == 0 = game { gameState = GameOver Nothing }
    | otherwise = game
    where board = gameBoard game

{-Implementação inicial da função, trocada pela utilização de Monad   
verifyGameState :: Game -> Game
verifyGameState game =
    case winner board of
        Just y  -> game { gameState = GameOver $ Just y }
        Nothing -> if isBoardFull board
                   then game { gameState = GameOver Nothing }
                   else game
  where
    board = gameBoard game

isBoardFull :: Board -> Bool
isBoardFull board = countCells Nothing board == 0
-}


{- 
    Realiza a jogada do jogador na coordenada especificada.
    Caso o jogador clique fora da grade ou em uma célula preenchida nada acontece
-}
playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | state == Running && isCoordValid cellCoord && isNothing (board ! cellCoord) =
        verifyGameState
        $ switchTurn
        $ game { gameBoard = board // [(cellCoord, Just player)] }
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game
          state = gameState game

