module Yaha.Compiller.Lexer where

import Text.Regex (mkRegex, matchRegexAll)
import Yaha.Templates

data Token = Token {
  ttype :: TokenType,
  value :: String
} deriving (Show)  

data TokenType = TNone | TInteger deriving (Show, Eq)

data LexicalRule = LexicalRule {
  pattern :: String,
  resultType :: TokenType
}

lr = LexicalRule

lexicalRules = [
  lr "\\s+" TNone,
  lr "[0-9]+" TInteger]

lexer :: String -> [Token]
lexer source = lexing 0 [] where
  sourceLen = length source
  lexing pos tokens
    | pos >= sourceLen = tokens
    | pos < sourceLen  = lexing newPos newTokensAcc where
        token = matchToken pos
        newPos = pos + (length $ value token)
        newTokensAcc = tokens ++ if (ttype token) == TNone
          then []
          else [token]

  matchToken pos = mappingRules 0 where
    sourceChunk = drop pos source
    rulesLen = length lexicalRules
    mappingRules i
      | i >= rulesLen = error (unexpectedSymErr (head sourceChunk) (pos + 1))
      | i < rulesLen  = token where
          rule = lexicalRules!!i
          regex = createRuleRegex rule
          result = matchRegexAll regex sourceChunk

          token = case result of
            Nothing  -> mappingRules $ (i + 1)
            Just r -> Token (resultType rule) ((\(_, v, _, _) -> v) r)

createRuleRegex = mkRegex . ((++) $ "^") . pattern
