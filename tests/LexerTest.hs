module LexerTest where

import Test.HUnit
import Text.Printf
import Yaha.Compiller.Lexer

createLexerTest tokenType text = TestCase (assertEqual (printf "Test lexing %s" (show tokenType)) [Token tokenType text] (lexer text))
    
testLexingInteger = createLexerTest TInteger "*573"
