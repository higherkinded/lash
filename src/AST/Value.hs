module AST.Value
  ( Value (..)
  ) where

import Data.Text

data Value
  = StringVal Text
  | BoolVal Bool
  | IntVal Integer
