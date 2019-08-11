{-# LANGUAGE OverloadedStrings #-}
module AST.Expr
  ( Expr (..)
  ) where

import Data.Text

import AST.Value

-- | In Lash, abstract syntax tree consists of expressions combined with other
-- expressions by means of combinators which are considered to be expressions,
-- too.
data Expr
  -- | If the value is standalone, it is an expression, too.
  = Val Value
  -- | Run a program with given arguments.
  | EvArgs Text [Expr]
  -- | Assignment expression.
  | EvAsgn Text Expr
  -- | Discards the value of the first expression, acts much like `>>`.
  | Expr :>> Expr
  -- | Short-cirquits a pair of expressions. If the preceding expression returns
  -- falsey value, the second one isn't evaluated and the first value is
  -- returned. In the opposite case, evaluation returns the result of the last
  -- expression.
  | Expr :&& Expr
  -- | Opposite of `(:&&)`. Evaluates expressions until there's a truthy result.
  -- If that doesn't happen, returns the last falsey result.
  | Expr :|| Expr
  -- | Pipe. Sends the output of preceding expression to the next expression.
  | Expr :|  Expr
  -- | Sends the evaluation of the first expression to bg, evaluates the next.
  | Expr :&  Expr
  -- | Empty expression. Removes the need to define two forms of `&`.
  | Empty
  deriving (Eq, Show)

instance Semigroup Expr where a <> b = a :>> b
instance Monoid Expr where mempty = Empty
