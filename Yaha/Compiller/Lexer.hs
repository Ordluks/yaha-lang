module Yaha.Compiller.Lexer where

import Text.Regex (mkRegex, matchRegex)
import Yaha.Templates

data Token = Token {
  ttype :: TokenType,
  value :: String
} deriving (Show)  

data TokenType = TNone | TInteger deriving (Show)

data LexicalRule = LexicalRule {
  pattern :: String,
  resultType :: TokenType
}

lr = LexicalRule

lexicalRules = [
  lr "[0-9]+" TNone]

lexer :: String -> [Token]
lexer source = lexing 0 [] where
  sourceLen = length source
  lexing pos tokens
    | pos >= sourceLen = tokens
    | pos < sourceLen  = lexing newPos newTokensAcc where
        token = matchToken pos
        newPos = pos + (length $ value token)
        newTokensAcc = tokens ++ [token]

  matchToken pos = mappingRules 0 where
    sourceChunk = drop pos source
    rulesLen = length lexicalRules
    mappingRules i
      | i >= rulesLen = error (unexpectedSymErr (head source) pos)
      | i < rulesLen  = token where
          rule = lexicalRules!!i
          regex = createRuleRegex rule
          result = matchRegex regex sourceChunk

          next = mappingRules $ (i + 1)

          token = case result of
            Nothing  -> next
            Just r -> if 0 < length r
              then Token (resultType rule) (head r)
              else next

createRuleRegex = mkRegex . ((++) $ "^") . pattern
