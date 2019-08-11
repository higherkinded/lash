{-# LANGUAGE OverloadedStrings #-}
module Parser.Value
  ( intParser
  , boolParser
  , strParser
  , valueParser
  ) where

import AST
import Parser.Common

import Data.Text
import Data.Functor
import Control.Applicative
import Data.Attoparsec.Text

_staticStrParser, _dynStrParser, _wordParser :: Parser Value

_staticStrParser = fmap (StrVal . pack) $ q *> many' p <* q
  where q = oneOf "'"
        p = notOf "'"

-- TODO: Resolve variables in dynamic string
-- TODO: Support expressions in dynamic string
_dynStrParser = fmap (StrVal . pack) $ q *> many' p <* q
  where q = oneOf "\""
        p = escaped <|> notOf "\""

_wordParser = fmap (StrVal . pack) $ many' $ escaped <|> notOf " \t"

intParser, boolParser, strParser, valueParser :: Parser Value

intParser = IntVal <$> decimal

boolParser = fmap BoolVal
  $   asciiCI "true" $> True
  <|> asciiCI "false" $> False

strParser = _staticStrParser <|> _dynStrParser <|> _wordParser

valueParser = skipSpace *> valParser
  where valParser
          =   intParser
          <|> boolParser
          <|> strParser
