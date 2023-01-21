import Test.HUnit
import System.Exit
import LexerTest

tests = TestList [testLexingInteger]

main = do
  (Counts _ _ errs fails) <- runTestTT tests
  if (errs > 0 || fails > 0)
    then exitFailure
    else exitSuccess
