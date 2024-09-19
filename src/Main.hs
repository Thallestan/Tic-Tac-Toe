-- Jogo da Velha
-- Autores: Thalles Stanziola, Giovana Reis e Henrique Souza

module Main (verifyGameState
  , window
  , backgroundColor
  , main
  ) where

import Graphics.Gloss
import Control.Monad.State
import Game
import Logic
import Interface
import Input (handleEvent, autoSolve)
import System.Environment (getArgs)
import System.Exit (exitFailure)

window :: Display
window = InWindow "Jogo da Velha Recursivo Arbitrário" (1300, 768) (700, -50)
-- Define a janela do jogo com título "Jogo da Velha", dimensões 1300x768 pixels e posição inicial (700, -50).

backgroundColor :: Color
backgroundColor = makeColor 0 0 0 255
-- Define a cor de fundo da janela como preto.

handleArgs :: [String] -> IO ()
handleArgs [] = play window backgroundColor 30 (initialGame 1) gameAsPicture handleEvent (const id)
handleArgs ("autoSolve" : depth : _) = do
    solvedGame <- execStateT autoSolve (initialGame $ read depth)
    play window backgroundColor 30 solvedGame gameAsPicture handleEvent (const id)
handleArgs (depth : _) = play window backgroundColor 30 (initialGame $ read depth) gameAsPicture handleEvent (const id)
-- A função 'handleArgs' lida com os argumentos da linha de comando:
-- Sem argumentos: inicia o jogo normalmente.
-- Com argumento "autoSolve": resolve o jogo automaticamente e inicia com o estado resolvido.
-- Qualquer outro argumento: exibe uma mensagem de erro e encerra o programa.

main :: IO ()
main = do
    args <- getArgs
    handleArgs args
-- A função 'main' é o ponto de entrada do programa:
-- Obtém os argumentos da linha de comando e chama 'handleArgs' para lidar com eles.
-- Define o estado inicial do jogo e executa uma sequência de jogadas.
-- Imprime o estado final do jogo.
