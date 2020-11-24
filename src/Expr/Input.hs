module Expr.Input where

import Lib (Id)

data Expr
  = Var Id
  | Ap Expr Expr
  deriving (Show)
