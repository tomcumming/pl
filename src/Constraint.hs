module Constraint where

import Lib (Id)
import qualified Type

data Expr
  = Var Id
  | Const Id
  | Ap Expr Expr
  deriving (Show)

matches :: Expr -> Type.Type -> Bool
matches e t = case (e, t) of
  (Var _, _) -> True
  (Const x, Type.Const y) -> x == y
  (Ap e1 e2, Type.Ap t1 t2) -> matches e1 t1 && matches e2 t2
  _ -> False
