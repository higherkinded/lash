{-# LANGUAGE OverloadedStrings #-}
module AST.Expr
  ( Expr (..)
  ) where

import Data.Text

import AST.Value

data Expr
  -- Atomic expressions
  = Eval   Text
  | EvArgs Text [Value]
  | EvAsgn Text Value
  -- Combinators
  | Expr   :>> Expr
  | Expr   :&& Expr
  | Expr   :|| Expr
  | Expr   :|  Expr
  | Expr   :&  Expr
  -- Empty expression
  | NoExpr
