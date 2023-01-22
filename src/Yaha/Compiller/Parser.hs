module Yaha.Compiller.Parser where

import Yaha.Compiller.Lexer

class SyntaxScaner where
  scanToken :: Token -> SyntaxScanStatus

data SyntaxScanStatus = Next | Repeat | Stop

data SyntaxRule = SyntaxRule [SyntaxScaner] (a -> b)

data ExpectToken = ExpectToken Token
instance SyntaxScaner ExpectToken where
  scanToken token = 

psrser tokens = parsing 0 where
  psrsing i = 

scan 
