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