{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module AST
  (
  ) where

import Prelude                hiding (takeWhile)
import Control.Applicative    ((<|>))
import Data.Functor           (($>))
import Data.Attoparsec.Text
import Data.Text              hiding (takeWhile)

data Value
  = Str Text
  | Number Integer
  | Boolean Bool
  deriving (Eq, Show)

data Expr a
  = Expr a Value
  | NoExpr
  deriving (Eq, Show)

data Operator a b
  = (Expr a) :>> (Expr b)
  | (Expr a) :&& (Expr b)
  | (Expr a) :|| (Expr b)
  | (Expr a) :|  (Expr b)
  | (Expr a) :&  (Expr b)
  deriving (Eq, Show)

numParser :: Parser Value
numParser = Number <$> decimal

boolParser :: Parser Value
boolParser = fmap Boolean
  $   asciiCI "true"  $> True
  <|> asciiCI "false" $> False

_genStrParser :: Char -> Parser Value
_genStrParser quot = Str <$> (char quot *> takeTill (== quot) <* char quot)

_staticStrParser :: Parser Value
_staticStrParser = _genStrParser '\''

_dynStrParser :: Parser Value
_dynStrParser = _genStrParser '\"'

_wordParser :: Parser Value
_wordParser = undefined

strParser :: Parser Value
strParser = _staticStrParser
  <|> _dynStrParser

valueParser :: Parser Value
valueParser = skipSpace *> valParser
  where valParser = numParser
          <|> boolParser
          <|> strParser
