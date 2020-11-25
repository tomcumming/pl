module Expr.Output where

import qualified Kind
import Lib (Id)
import qualified Type

data Expr
  = Var Id
  | Ap Expr Expr
  | Abs Id Type.Type Expr [Id]
  | TypeAp Expr Type.Type
  | TypeAbs Id Kind.Kind Expr
  deriving (Show)
