module Game where

-- Importa a biblioteca Data.Array para manipulação de arrays.
import Data.Array

-- Define o tipo Player com dois valores possíveis: PlayerX e PlayerO.
data Player = PlayerX | PlayerO deriving (Eq, Show)

-- Define o tipo Cell como um Maybe Player, que pode ser Nothing ou Just Player.
type Cell = Maybe Player

-- Define o tipo State com dois valores possíveis: Running e GameOver (com um Maybe Player).
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

-- Define o tipo Board como um array de células indexado por tuplas de inteiros.
type Board = Array (Int, Int) Cell

-- Define o tipo Game com três campos: gameBoard, gamePlayer e gameState.
data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)

-- Define a dimensão do tabuleiro.
n :: Int
n = 3

-- Define a largura da tela.
screenWidth :: Int
screenWidth = 640

-- Define a altura da tela.
screenHeight :: Int
screenHeight = 480

-- Calcula a largura de cada célula com base na largura da tela e na dimensão do tabuleiro.
cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

-- Calcula a altura de cada célula com base na altura da tela e na dimensão do tabuleiro.
cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

-- Inicializa o jogo com um tabuleiro vazio, PlayerX como jogador inicial e estado Running.
initialGame :: Game
initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , gamePlayer = PlayerX
                   , gameState = Running
                   }
    where indexRange = ((0, 0), (n - 1, n - 1))
