module Expr.Input where

import Lib (Id)

data Expr
  = Var Id
  | Ap Expr Expr
  | Abs Id Expr
  deriving (Show)
