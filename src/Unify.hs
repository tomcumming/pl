module Unify where

import Check.Error (Error)
import Check.Kind (kind)
import Control.Monad (when)
import qualified Ctx
import qualified Data.Map as Map
import qualified Type

unify :: Ctx.Ctx -> Type.Type -> Type.Type -> Either Error Ctx.Ctx
unify ctx t1 t2 = do
  let t1 = Ctx.apply ctx t1
  let t2 = Ctx.apply ctx t2
  k1 <- kind ctx t1
  k2 <- kind ctx t2

  when (k1 /= k2) (Left $ unwords ["Can't unify; different kinds", show t1, show t2])

  case (t1, t2) of
    (Type.Var v1, Type.Var v2) -> unifyVars ctx v1 v2
    (Type.Var v, Type.Const c) -> undefined

unifyVars :: Ctx.Ctx -> Type.Var -> Type.Var -> Either Error Ctx.Ctx
unifyVars ctx v1 v2 =
  if v1 == v2
    then Right ctx
    else case (Map.lookup v1 (Ctx.typeVars ctx), Map.lookup v2 (Ctx.typeVars ctx)) of
      (Just (l1, _), Just (l2, _)) ->
        if l1 <= l2
          then addSub ctx v2 (Type.Var v1)
          else addSub ctx v1 (Type.Var v2)
      _ -> Left $ unwords ["Missing a type var from", show v1, show v2]

addSub :: Ctx.Ctx -> Type.Var -> Type.Type -> Either String Ctx.Ctx
addSub ctx v t = undefined
