module Main where

import Test.HUnit
import LexerTest

tests = TestList [testLexingInteger]

main = runTestTT tests
