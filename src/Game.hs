module Game where

import Data.Array

data Player = X | O deriving (Eq, Show)
-- Define um tipo de dado 'Player' que pode ser 'X' ou 'O'.
-- A derivação 'Eq' permite comparar jogadores, e 'Show' permite exibir jogadores como strings.

type Cell = Maybe Player
-- Define 'Cell' como um tipo que pode ser 'Nothing' (célula vazia) ou 'Just Player' (célula ocupada por um jogador).

data Estado = Running | GameOver (Maybe Player) deriving (Eq, Show)
-- Define um tipo de dado 'Estado' que pode ser 'Running' (jogo em andamento) ou 'GameOver (Maybe Player)' (jogo terminado, com um possível vencedor).
-- A derivação 'Eq' permite comparar estados, e 'Show' permite exibir estados como strings.

type Board = Array (Int, Int) Cell
-- Define 'Board' como um array bidimensional de células ('Cell'), indexado por tuplas de inteiros.

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: Estado
                 } deriving (Eq, Show)
-- Define um tipo de dado 'Game' que contém o tabuleiro ('gameBoard'), o jogador atual ('gamePlayer') e o estado do jogo ('gameState').
-- A derivação 'Eq' permite comparar jogos, e 'Show' permite exibir jogos como strings.

boardDimension :: Int
boardDimension = 3
-- Define a dimensão do tabuleiro como 3 (um tabuleiro 3x3).

screenWidth :: Int
screenWidth = 640
-- Define a largura da tela como 640 pixels.

screenHeight :: Int
screenHeight = 480
-- Define a altura da tela como 480 pixels.

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral boardDimension
-- Calcula a largura de cada célula do tabuleiro, convertendo 'screenWidth' e 'boardDimension' para 'Float' e dividindo-os.

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral boardDimension
-- Calcula a altura de cada célula do tabuleiro, convertendo 'screenHeight' e 'boardDimension' para 'Float' e dividindo-os.

initialGame :: Game
initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , gamePlayer = X
                   , gameState = Running
                   }
    where indexRange = ((0, 0), (boardDimension - 1, boardDimension - 1))
-- Define o estado inicial do jogo ('initialGame'):
-- 'gameBoard': Cria um array de células vazias ('Nothing') com índices de '(0, 0)' a '(2, 2)'.
-- 'gamePlayer': Define o jogador inicial como 'X'.
-- 'gameState': Define o estado inicial do jogo como 'Running'.
-- 'indexRange': Define o intervalo de índices do tabuleiro.
