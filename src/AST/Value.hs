module AST.Value
  ( Value (..)
  ) where

import Data.Text

data Value
  = StrVal    Text
  | BoolVal   Bool
  | IntVal    Integer
  deriving (Eq, Show)
