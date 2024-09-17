-- Jogo da Velha
-- Autores: Thalles Stanziola, Giovana Reis e Henrique Souza

module Main (main) where
-- Define o módulo 'Main', exportando a função 'main'.

import Graphics.Gloss
-- Importa o módulo 'Graphics.Gloss' para renderização gráfica.

import Control.Monad.State
-- Importa o módulo 'Control.Monad.State', que fornece a monada 'State' para manipulação de estado.

import Game
-- Importa o módulo 'Game', que contém definições e funções relacionadas ao jogo.

import Logic
-- Importa o módulo 'Logic', que contém a lógica do jogo.

import Interface
-- Importa o módulo 'Interface', que provavelmente contém funções para a interface do usuário.

import Input (handleEvent, autoSolve)
-- Importa as funções 'handleEvent' e 'autoSolve' do módulo 'Input', que lidam com eventos de entrada e resolução automática do jogo.

import System.Environment (getArgs)
-- Importa a função 'getArgs' do módulo 'System.Environment' para obter argumentos da linha de comando.

import System.Exit (exitFailure)
-- Importa a função 'exitFailure' do módulo 'System.Exit' para encerrar o programa com falha.

window :: Display
window = InWindow "Jogo da Velha" (850, 768) (700, -50)
-- Define a janela do jogo com título "Jogo da Velha", dimensões 850x768 pixels e posição inicial (700, -50).

backgroundColor :: Color
backgroundColor = makeColor 0 0 0 255
-- Define a cor de fundo da janela como preto.

handleArgs :: [String] -> IO ()
handleArgs [] = play window backgroundColor 30 initialGame gameAsPicture handleEvent (const id)
handleArgs ("autoSolve" : _) = do
    solvedGame <- execStateT autoSolve initialGame
    play window backgroundColor 30 solvedGame gameAsPicture handleEvent (const id)
handleArgs _ = do
    putStrLn "An unexpected error occurred."
    exitFailure
-- A função 'handleArgs' lida com os argumentos da linha de comando:
-- Sem argumentos: inicia o jogo normalmente.
-- Com argumento "autoSolve": resolve o jogo automaticamente e inicia com o estado resolvido.
-- Qualquer outro argumento: exibe uma mensagem de erro e encerra o programa.

runGame :: State Game () -> Game -> Game
runGame gameAction initialGame = execState gameAction initialGame
-- A função 'runGame' executa uma ação de jogo no estado inicial e retorna o estado final do jogo.

main :: IO ()
main = do
    args <- getArgs
    handleArgs args
    let game = initialGame
        finalGame = runGame (playerTurn (0, 0) >> playerTurn (1, 1) >> playerTurn (0, 1) >> playerTurn (1, 0) >> playerTurn (0, 2)) game
    print finalGame
-- A função 'main' é o ponto de entrada do programa:
-- Obtém os argumentos da linha de comando e chama 'handleArgs' para lidar com eles.
-- Define o estado inicial do jogo e executa uma sequência de jogadas.
-- Imprime o estado final do jogo.
