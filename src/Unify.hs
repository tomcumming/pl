module Unify where

import qualified Check.Ctx
import Check.Error (Error)
import Check.Kind (kind)
import Control.Monad (when)
import qualified Ctx.Local as Local
import qualified Type

unify :: Local.Ctx -> Type.Type -> Type.Type -> Either Error Local.Ctx
unify ctx t1 t2 = do
  t1 <- return $ Local.apply ctx t1
  t2 <- return $ Local.apply ctx t2
  case (t1, t2) of
    (Type.Var v1, Type.Var v2) -> unifyVars ctx v1 v2
    (Type.Var v, Type.Const c) -> solve ctx v (Type.Const c)
    (Type.Const c, Type.Var v) -> solve ctx v (Type.Const c)
    (Type.Var v, Type.Fn) -> solve ctx v Type.Fn
    (Type.Fn, Type.Var v) -> solve ctx v Type.Fn
    (Type.Var v, Type.Ap tf ta) -> unifyAp ctx v tf ta
    (Type.Ap tf ta, Type.Var v) -> unifyAp ctx v tf ta
    (Type.Const x, Type.Const y) | x == y -> Right ctx
    (Type.Fn, Type.Fn) -> Right ctx
    (Type.Ap t1f t1a, Type.Ap t2f t2a) -> do
      ctx <- unify ctx t1f t2f
      unify ctx t1a t2a
    (Type.Forall x1 k1 t1, Type.Forall x2 k2 t2) -> Left "TODO: unify forall"
    (t1, t2) -> Left $ unwords ["Cant unify", show t1, show t2]

unifyVars :: Local.Ctx -> Type.Var -> Type.Var -> Either Error Local.Ctx
unifyVars ctx v1 v2 =
  if v1 == v2
    then Right ctx
    else do
      (v1, v2) <- Check.Ctx.orderVars ctx v1 v2
      solve ctx v1 (Type.Var v2)

unifyAp :: Local.Ctx -> Type.Var -> Type.Type -> Type.Type -> Either Error Local.Ctx
unifyAp ctx v tf ta = do
  tfk <- kind ctx tf
  tak <- kind ctx ta
  (ctx, vf) <- return $ Local.fresh ctx
  (ctx, va) <- return $ Local.fresh ctx
  ctx <-
    return $
      Local.insertBehind
        ctx
        v
        [Local.TypeVar vf (Local.Unsolved tfk), Local.TypeVar va (Local.Unsolved tak)]
  ctx <- solve ctx v (Type.Ap (Type.Var vf) (Type.Var va))
  ctx <- unify ctx (Type.Var vf) tf
  unify ctx (Type.Var va) ta

solve :: Local.Ctx -> Type.Var -> Type.Type -> Either Error Local.Ctx
solve ctx v t = do
  vk <- kind ctx (Type.Var v)
  tk <- kind ctx t

  when (vk /= tk) $ Left $ unwords ["Wrong kinds", show v, show vk, show t, show tk]

  -- TODO: check that v not in t

  go [] (Local.parts ctx)
  where
    go ctxHead ctxTail = case ctxTail of
      [] -> Left $ "Could not find unsolved: " ++ show v
      Local.TypeVar u tv : ctxTail | u == v -> case tv of
        Local.Solved _ -> Left $ "Already solved!?"
        Local.Unsolved _ -> do
          kind (ctx {Local.parts = ctxTail}) t
          Check.Ctx.checkMono ctx t
          let parts = reverse ctxHead ++ Local.TypeVar v (Local.Solved t) : ctxTail
          Right $ ctx {Local.parts = parts}
      p : ctxTail -> go (p : ctxHead) ctxTail
