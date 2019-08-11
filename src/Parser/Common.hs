module Parser.Common
  ( escaped
  , classRelation
  , escClassRelation
  , oneOf
  , notOf
  ) where

import Util.Common

import Control.Applicative
import Data.Attoparsec.Text

escaped :: Parser Char
escaped = char '\\' *> anyChar

classRelation :: String -> Predicate Bool -> Parser Char
classRelation c = satisfy . (<$> inClass c)

escClassRelation :: String -> Predicate Bool -> Parser Char
escClassRelation c f = escaped <|> classRelation c f

oneOf, notOf :: String -> Parser Char
oneOf = flip classRelation id
notOf = flip classRelation not
