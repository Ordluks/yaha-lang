module Main where

import Yaha.Compiller.Lexer (lexer)

main :: IO ()
main = print (lexer "105")
