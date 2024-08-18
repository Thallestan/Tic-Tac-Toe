-- Jogo da Velha
-- Autores: Thalles Stanziola, Giovana Reis e Henrique Souza

module Main (verifyGameState
  , window
  , backgroundColor
  , main
  ) where

-- Importa a biblioteca Graphics.Gloss para renderização gráfica.
import Graphics.Gloss

-- Importa módulos personalizados do jogo.
import Game
import Logic
import Interface
import Input (handleEvent, autoSolve)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- Define a janela de exibição com título, dimensões e posição na tela.
window :: Display
window = InWindow "Jogo da Velha" (850, 768) (700, -50)

-- Define a cor de fundo da janela como preto.
backgroundColor :: Color
backgroundColor = makeColor 0 0 0 255

handleArgs :: [String] -> IO ()
handleArgs [] = play window backgroundColor 30 initialGame gameAsPicture handleEvent (const id)
handleArgs ("autoSolve" : _) = do
    solvedGame <- autoSolve initialGame
    play window backgroundColor 30 solvedGame gameAsPicture handleEvent (const id)
handleArgs _ = do
    putStrLn "An unexpected error occurred."
    exitFailure

-- Função principal que inicia o jogo.
main :: IO ()
main = do
    args <- getArgs
    handleArgs args
