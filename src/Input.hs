module Input where
import Game (Game (gameState), State (Running, GameOver), screenHeight, cellHeight, screenWidth, cellWidth, initialGame)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (MouseButton), MouseButton (LeftButton), KeyState (Up))
import Logic (playerTurn)

-- Converte a posição do mouse em coordenadas de célula.
mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )

--Transforma o estado do jogo com base no evento recebido.
handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> playerTurn game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame
handleEvent _ game = game
