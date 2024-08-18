module Interface (gameAsPicture) where

{-
Import de bibliotecas padrões Haskell
    Data.Array para manipulação de arrays;
    Graphics.Gloss para renderização gráfica.
-}

import Data.Array
import Graphics.Gloss

-- Importa módulos personalizados do jogo.
import Game


-- Define a cor da grade do tabuleiro.
boardGridColor :: Color
boardGridColor = makeColorI 255 255 255 255

-- Define a cor do jogador X.
playerXColor :: Color
playerXColor = makeColorI 255 50 50 255

-- Define a cor do jogador O.
playerOColor :: Color
playerOColor = makeColorI 50 100 255 255

-- Define a cor para empate.
tieColor :: Color
tieColor = greyN 0.5

-- Constrói a imagem do tabuleiro durante o jogo.
boardAsRunningPicture :: Game -> Picture
boardAsRunningPicture game =
    pictures [ color playerXColor $ xCellsOfBoard (gameBoard game)
             , color playerOColor $ oCellsOfBoard (gameBoard game)
             , color boardGridColor boardGrid
             , color boardGridColor $ translate (60) (600) $
                         scale 0.5 0.5 $ text ("Turno do " ++ (show (gamePlayer game)))
             ]

-- Define a cor do resultado do jogo com base no vencedor.
outcomeColor :: Maybe Player -> Color
outcomeColor (Just X) = playerXColor
outcomeColor (Just O) = playerOColor
outcomeColor Nothing = tieColor

-- Ajusta a imagem para a célula especificada.
snapPictureToCell :: (Integral a, Integral b) => Picture -> (a, b) -> Picture
snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

-- Define a imagem da célula X. COnstruída a partir de retângulos sobrepostos.
xCell :: Picture
xCell = pictures [ rotate 45.0 $ rectangleSolid side 10.0
                 , rotate (-45.0) $ rectangleSolid side 10.0
                 ]
    where side = min cellWidth cellHeight * 0.75

-- Define a imagem da célula O. Construída com a função de criação de círculos.
oCell :: Picture
oCell = thickCircle radius 10.0
    where radius = min cellWidth cellHeight * 0.25

-- Constrói a imagem das células do tabuleiro.
cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

-- Constrói a imagem das células X do tabuleiro.
xCellsOfBoard :: Board -> Picture
xCellsOfBoard board = cellsOfBoard board (Just X) xCell

-- Constrói a imagem das células O do tabuleiro.
oCellsOfBoard :: Board -> Picture
oCellsOfBoard board = cellsOfBoard board (Just O) oCell

-- Constrói a grade do tabuleiro.
boardGrid :: Picture
boardGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral boardDimension]

-- Constrói a imagem do tabuleiro quando o jogo termina.
boardAsGameOverPicture :: Maybe Player -> Game -> Picture
boardAsGameOverPicture winner game = 
            pictures [color playerXColor $ xCellsOfBoard (gameBoard game)
             , color playerOColor $ oCellsOfBoard (gameBoard game)
             , color boardGridColor boardGrid
             , color (outcomeColor winner) $ translate (60) (600) $ scale 0.5 0.5 $ text (gameOverText winner)
             ]

-- Define o texto apresentado ao final do jogo
gameOverText :: Maybe Player -> String
gameOverText (Just y) = show y ++ " Venceu" 
gameOverText Nothing = "Deu Velha!"

-- Constrói a imagem do jogo com base no estado atual.
gameAsPicture :: Game -> Picture
gameAsPicture game = 
            case gameState game of
                    Running -> translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5)) $ boardAsRunningPicture game
                    GameOver winner -> translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5)) $ boardAsGameOverPicture winner game
