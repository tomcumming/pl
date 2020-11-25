module Type where

import Kind (Kind)
import Lib (Id)

type Var = Word

data Type
  = Var Var
  | Const Id
  | Fn
  | Ap Type Type
  | Forall Id Kind Type
  deriving (Show)

fn :: Type -> Type -> Type -> Type
fn ctx arg = Type.Ap (Type.Ap (Type.Ap Type.Fn ctx) arg)

mono :: Type -> Bool
mono t = case t of
  Var _ -> True
  Const _ -> True
  Fn -> True
  Ap t t2 -> mono t && mono t2
  Forall {} -> False
