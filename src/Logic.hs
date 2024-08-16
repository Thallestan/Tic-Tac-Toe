module Logic where

-- Importa a biblioteca Data.Array para manipulação de arrays.
import Data.Array
-- Importa a função 'asum' da biblioteca Data.Foldable.
import Data.Foldable (asum)

-- Importa módulos personalizados do jogo.
import Game
-- Importa a interface gráfica do Gloss.
import Graphics.Gloss.Interface.Pure.Game
-- Importa a função 'isNothing' da biblioteca Data.Maybe.
import Data.Maybe (isNothing)


-- Verifica se a coordenada está dentro do intervalo permitido.
isCoordCorrect :: (Int, Int) -> Bool
isCoordCorrect = inRange ((0, 0), (n - 1, n - 1))

-- Alterna o jogador atual no jogo.
switchPlayer :: Game -> Game
switchPlayer game =
    case gamePlayer game of
      PlayerX -> game { gamePlayer = PlayerO }
      PlayerO -> game { gamePlayer = PlayerX }

-- Verifica se todas as células em uma lista são iguais e não vazias.
full :: [Cell] -> Maybe Player
full (cell@(Just player):cells) | all (== cell) cells = Just player
full _                                                = Nothing

-- Determina o vencedor do jogo verificando linhas, colunas e diagonais.
winner :: Board -> Maybe Player
winner board = asum $ map full $ rows ++ cols ++ diags
    where rows  = [[board ! (i,j) | i <- [0..n-1]] | j <- [0..n-1]]
          cols  = [[board ! (j,i) | i <- [0..n-1]] | j <- [0..n-1]]
          diags = [[board ! (i,i) | i <- [0..n-1]]
                  ,[board ! (i,j) | i <- [0..n-1], let j = n-1-i ]]

-- Conta o número de células específicas no tabuleiro.
countCells :: Cell -> Board -> Int
countCells cell = length . filter (cell ==) . elems

-- Verifica se o jogo terminou e atualiza o estado do jogo.
checkGameOver :: Game -> Game
checkGameOver game
    | Just p <- winner board =
        game { gameState = GameOver $ Just p }
    | countCells Nothing board == 0 =
        game { gameState = GameOver Nothing }
    | otherwise = game
    where board = gameBoard game

-- Realiza a jogada do jogador na coordenada especificada.
playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | isCoordCorrect cellCoord && isNothing (board ! cellCoord) =
        checkGameOver
        $ switchPlayer
        $ game { gameBoard = board // [(cellCoord, Just player)] }
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game

-- Converte a posição do mouse em coordenadas de célula.
mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )

--Transforma o estado do jogo com base no evento recebido.
transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> playerTurn game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame
transformGame _ game = game





