module Interface (gameAsPicture) where

{-
Import de bibliotecas padrões Haskell
    Data.Array para manipulação de arrays;
    Graphics.Gloss para renderização gráfica.
-}

import Graphics.Gloss

-- Importa módulos personalizados do jogo.
import Data.Maybe (catMaybes, maybeToList)
import Data.Matrix (toList, mapPos)
import Logic (winner)
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

boardToPicture :: Board -> Picture
boardToPicture board = pictures [
    boardGrid board,
    pictures $ playersPlacements board
  ]

playersPlacements :: Board -> [Picture]
playersPlacements = go
  where go :: Board -> [Picture]
        go (Leaf l) = [
            pictures $ catMaybes $ toList $ mapPos (
                \(i, j) cell -> do
                    c <- cell
                    Just $ translate
                        ((fromIntegral (i - 1) * cellWidth) + (cellWidth / 2))
                        ((fromIntegral (j - 1) * cellHeight) + (cellHeight / 2))
                      $ playerPicture c
            ) l]
        go (Node n) = [pictures $ toList $ mapPos (
                \(i, j) b ->
                    translate (fromIntegral (i - 1) * cellWidth) (fromIntegral (j - 1) * cellHeight) $
                    pictures (
                        scale (1 / 3) (1 / 3) (pictures $ go b) :
                        maybeToList (fmap (translate (cellWidth / 2) (cellHeight / 2) . playerPicture) (winner b))
                    )
                ) n]

playerPicture :: Player -> Picture
playerPicture X = xPicture
playerPicture O = oPicture

-- Constrói a imagem do tabuleiro durante o jogo.
boardAsRunningPicture :: Game -> Picture
boardAsRunningPicture game =
    pictures [boardToPicture board]
    where board = gameBoard game
          player = gamePlayer game

-- Define a cor do resultado do jogo com base no vencedor.
-- Aqui utilizamos a função maybe pré existente no prelude haskell que funciona de forma semelhante ao fmap do tipo Maybe
outcomeColor :: Maybe Player -> Color
outcomeColor = maybe tieColor playerColor
  where
    playerColor X = playerXColor
    playerColor O = playerOColor


xPictureSide :: Float
xPictureSide = min cellWidth cellHeight * 0.75

-- Define a imagem da célula X. Construída a partir de retângulos sobrepostos.
xPicture :: Picture
xPicture = color playerXColor $ pictures
    [
        rotate 45.0 $ rectangleSolid xPictureSide 20.0,
        rotate (-45.0) $ rectangleSolid xPictureSide 20.0
    ]

-- Define a imagem da célula O. Construída com a função de criação de círculos.
oPicture :: Picture
oPicture = color playerOColor $ thickCircle radius 20.0
    where radius = min cellWidth cellHeight * 0.25

-- Constrói a grade do tabuleiro.
boardGrid :: Board -> Picture
boardGrid board =
    color boardGridColor $ pictures $ go board
      where go :: Board -> [Picture]
            go (Leaf _) = [
                pictures $ concatMap (\li -> [
                    line [(li * cellWidth, 0.0), (li * cellWidth, fromIntegral screenHeight)],
                    line [(0.0, li * cellHeight), (fromIntegral screenWidth, li * cellHeight)]
                ]) [1.0 .. fromIntegral boardDimension - 1]]
            go (Node n) = [
                    pictures $ concatMap (\li -> [
                        translate (fromIntegral screenWidth / 2) (li * cellHeight) $ rectangleSolid (fromIntegral screenWidth) 8.0,
                        translate (li * cellWidth) (fromIntegral screenHeight / 2) $ rectangleSolid 8.0 (fromIntegral screenHeight)
                    ]) [1.0 .. fromIntegral boardDimension - 1] ++ [
                        pictures $
                        toList $
                        mapPos (
                            \(i, j) b ->
                                translate (fromIntegral (i - 1) * cellWidth) (fromIntegral (j - 1) * cellHeight) $
                                scale (1 / 3) (1 / 3) $
                                pictures $
                                go b
                        ) n
                    ]
                ]

-- Constrói a imagem do tabuleiro quando o jogo termina.
boardAsGameOverPicture :: Maybe Player -> Game -> Picture
boardAsGameOverPicture theWinner game =
    pictures $ boardToPicture board : [color (outcomeColor theWinner) $ translate (fromIntegral screenWidth * 0.5 - 155) (fromIntegral screenHeight + 40) $ scale 0.5 0.5 $ text (gameOverText theWinner)]
    where board = gameBoard game

-- Define o texto apresentado ao final do jogo
gameOverText :: Maybe Player -> String
gameOverText (Just y) = show y ++ " Venceu"
gameOverText Nothing = "Deu Velha!"

-- Constrói a imagem do jogo com base no estado atual.
gameAsPicture :: Game -> Picture
gameAsPicture game = do
    case gameState game of
        Running -> translate (fromIntegral screenWidth * (-0.5)) (fromIntegral screenHeight * (-0.5)) $ boardAsRunningPicture game
        GameOver theWinner -> translate (fromIntegral screenWidth * (-0.5)) (fromIntegral screenHeight * (-0.5)) $ boardAsGameOverPicture theWinner game

