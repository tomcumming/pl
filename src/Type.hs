module Type where

import qualified Data.Set as Set
import Kind (Kind)
import Lib (Id)

type Var = Word

data Type
  = Var Var
  | Const Id
  | Fn
  | Ap Type Type
  | Forall Id Kind Type
  deriving (Show, Eq)

fn :: Type -> Type -> Type -> Type
fn ctx arg = Type.Ap (Type.Ap (Type.Ap Type.Fn ctx) arg)

free :: Type -> Set.Set Type.Var
free t = case t of
  Var v -> Set.singleton v
  Const _ -> Set.empty
  Fn -> Set.empty
  Ap t t2 -> Set.union (free t) (free t2)
  Forall _ _ t -> free t

mono :: Type -> Bool
mono t = case t of
  Var _ -> True
  Const _ -> True
  Fn -> True
  Ap t t2 -> mono t && mono t2
  Forall {} -> False

subsConst :: Id -> Type -> Type -> Type
subsConst x t t2 = case t of
  Var v -> Var v
  Const y -> if x == y then t2 else Const y
  Fn -> Fn
  Ap tf ta -> Ap (subsConst x tf t2) (subsConst x ta t2)
  Forall y k t -> if x == y then Forall y k t else Forall y k (subsConst x t t2)

safeFreshForallName :: Id -> Type -> Bool
safeFreshForallName x t = case t of
  Var _ -> True
  Const y -> x /= y
  Fn -> True
  Ap t t2 -> safeFreshForallName x t && safeFreshForallName x t2
  Forall y _ t -> x /= y && safeFreshForallName x t
