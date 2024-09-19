module Game where

import Data.Matrix

data Player = X | O deriving (Eq, Show)
-- Define um tipo de dado 'Player' que pode ser 'X' ou 'O'.
-- A derivação 'Eq' permite comparar jogadores, e 'Show' permite exibir jogadores como strings.

type Cell = Maybe Player
-- Define 'Cell' como um tipo que pode ser 'Nothing' (célula vazia) ou 'Just Player' (célula ocupada por um jogador).

data Estado = Running | GameOver (Maybe Player) deriving (Eq, Show)
-- Define um tipo de dado 'Estado' que pode ser 'Running' (jogo em andamento) ou 'GameOver (Maybe Player)' (jogo terminado, com um possível vencedor).
-- A derivação 'Eq' permite comparar estados, e 'Show' permite exibir estados como strings.

data Tree a = Node (Matrix (Tree a)) | Leaf a
    deriving (Show, Eq)

treeDepth :: Tree (Matrix a) -> Int
treeDepth (Leaf _) = 0
treeDepth (Node n) = 1 + maximum (treeDepth <$> toList n)

instance Functor Tree where
    fmap f (Leaf l) = Leaf (f l)
    fmap f (Node n) = Node (fmap (fmap f) n)

type Board = Tree (Matrix Cell)

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

indexRange :: ((Int, Int), (Int, Int))
indexRange = ((0, 0), (boardDimension - 1, boardDimension - 1))

emptyBoard :: Int -> Board
emptyBoard 1 = Leaf $ matrix boardDimension boardDimension $ const Nothing
emptyBoard depth = Node $ matrix boardDimension boardDimension $ const (emptyBoard (depth - 1))

setNodeElem :: Tree (Matrix Cell) -> (Int, Int) -> Board -> Board
setNodeElem e pos (Node n) = Node $ setElem e pos n
setNodeElem _ _ board = board

initialGame :: Int -> Game
initialGame depth = Game { gameBoard = emptyBoard depth
                   , gamePlayer = X
                   , gameState = Running
                   }
