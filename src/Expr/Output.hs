module Expr.Output where

import qualified Data.Set as Set
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

usedVars :: Expr -> Set.Set Id
usedVars e = case e of
  Var x -> Set.singleton x
  Ap e1 e2 -> Set.union (usedVars e1) (usedVars e2)
  Abs _ _ _ cs -> Set.fromList cs
  TypeAp e _ -> usedVars e
  TypeAbs _ _ e -> usedVars e
