module Ctx where

import qualified Data.Map as Map
import qualified Kind
import Lib (Id)
import qualified Type

type Level = Word

data Ctx = Ctx
  { typeConsts :: Map.Map Id Kind.Kind,
    typeVars :: Map.Map Type.Var (Level, Kind.Kind),
    typeSubs :: Map.Map Type.Var Type.Type, -- needs level, must be monotype?
    named :: Map.Map Id Type.Type,
    level :: Level
  }
  deriving (Show)

apply :: Ctx -> Type.Type -> Type.Type
apply ctx t = case t of
  Type.Var v | Just t <- Map.lookup v (typeSubs ctx) -> t
  Type.Ap tFn tArg -> Type.Ap (apply ctx tFn) (apply ctx tArg)
  Type.Forall x k t -> Type.Forall x k (apply ctx t)
  t -> t
