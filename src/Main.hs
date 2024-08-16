-- Jogo da Velha
-- Autores: Thalles Stanziola, Giovana Reis e Henrique Souza

module Main where

-- Importa a biblioteca Graphics.Gloss para renderização gráfica.
import Graphics.Gloss

-- Importa módulos personalizados do jogo.
import Game
import Logic
import Rendering

-- Define a janela de exibição com título, dimensões e posição na tela.
window :: Display
window = InWindow "Functional" (screenWidth, screenHeight) (100, 100)

-- Define a cor de fundo da janela como preto.
backgroundColor :: Color
backgroundColor = makeColor 0 0 0 255

-- Função principal que inicia o jogo.
main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
