module Unify (unify) where

import qualified Check.Ctx
import Check.Error (Error)
import Check.Kind (kind)
import Control.Monad (when)
import qualified Ctx.Global as Global
import qualified Ctx.Local as Local
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Kind
import Lib (Id)
import qualified Type

unify :: Global.Ctx -> Local.Ctx -> Type.Type -> Type.Type -> Either Error Local.Ctx
unify gctx ctx t1 t2 = case (Local.apply ctx t1, Local.apply ctx t2) of
  -- unify var
  (Type.Var v1, Type.Var v2) -> unifyVars gctx ctx v1 v2
  (Type.Var v, Type.Const c) -> unifyConst gctx ctx v c
  (Type.Const c, Type.Var v) -> unifyConst gctx ctx v c
  (Type.Var v, Type.Fn) -> solve gctx ctx v Type.Fn
  (Type.Fn, Type.Var v) -> solve gctx ctx v Type.Fn
  (Type.Var v, Type.Ap tf ta) -> unifyAp gctx ctx v tf ta
  (Type.Ap tf ta, Type.Var v) -> unifyAp gctx ctx v tf ta
  -- unify eq
  (Type.Const x, Type.Const y) | x == y -> Right ctx
  (Type.Fn, Type.Fn) -> Right ctx
  (Type.Ap t1f t1a, Type.Ap t2f t2a) -> do
    ctx <- unify gctx ctx t1f t2f
    unify gctx ctx t1a t2a
  -- Foralls
  (Type.Forall x1 k1 t1, Type.Forall x2 k2 t2) -> unifyForalls gctx ctx (x1, k1, t1) (x2, k2, t2)
  --
  (t1, t2) -> Left $ unwords ["Cant unify", show t1, show t2]

unifyVars :: Global.Ctx -> Local.Ctx -> Type.Var -> Type.Var -> Either Error Local.Ctx
unifyVars gctx ctx v1 v2 =
  if v1 == v2
    then Right ctx
    else case (Local.lookupVar ctx v1, Local.lookupVar ctx v2) of
      ((l1, _, _), (l2, _, _)) | l1 <= l2 -> solve gctx ctx v2 (Type.Var v1)
      _ -> solve gctx ctx v1 (Type.Var v2)

unifyConst :: Global.Ctx -> Local.Ctx -> Type.Var -> Id -> Either Error Local.Ctx
unifyConst gctx ctx v c = do
  cl <- Check.Ctx.constLevel gctx ctx c
  case (cl, Local.lookupVar ctx v) of
    (cl, (vl, _, _)) | cl > vl -> Left $ "Type variable can escape forall: " ++ c
    _ -> solve gctx ctx v (Type.Const c)

unifyAp :: Global.Ctx -> Local.Ctx -> Type.Var -> Type.Type -> Type.Type -> Either Error Local.Ctx
unifyAp gctx ctx v tf ta = do
  tfk <- kind gctx ctx tf
  tak <- kind gctx ctx ta
  (ctx, vf) <- return $ Local.addVar ctx tfk
  (ctx, va) <- return $ Local.addVar ctx tak
  ctx <- solve gctx ctx v (Type.Ap (Type.Var vf) (Type.Var va))
  ctx <- unify gctx ctx (Type.Var vf) tf
  unify gctx ctx (Type.Var va) ta

unifyForalls ::
  Global.Ctx ->
  Local.Ctx ->
  (Id, Kind.Kind, Type.Type) ->
  (Id, Kind.Kind, Type.Type) ->
  Either Error Local.Ctx
unifyForalls gctx ctx (x1, k1, t1) (x2, k2, t2) =
  if k1 /= k2
    then Left $ unwords ["Can't unify forall, wrong kinds", show k1, show k2]
    else do
      let x3 = head $ filter (\x -> Type.safeFreshForallName x (Type.Forall x2 k2 t2)) (freshForallNames x1)
      t1 <- return $ Type.subsConst x1 t1 (Type.Const x3)
      t2 <- return $ Type.subsConst x2 t2 (Type.Const x3)
      unify gctx ctx t1 t2

solve :: Global.Ctx -> Local.Ctx -> Type.Var -> Type.Type -> Either Error Local.Ctx
solve gctx ctx v t = do
  tk <- kind gctx ctx t
  case Local.lookupVar ctx v of
    (_, _, Just t2) -> Left $ unwords ["Already solved:", show t2, show t]
    _ | not (Type.mono t) -> Left $ unwords ["Can't solve as polymorphic'", show v, show t]
    (_, k, Nothing) | k /= tk -> Left $ unwords ["Wrong kinds", show v, show k, show t, show tk]
    _ -> do
      ctx <- return $ ctx {Local.solvedVars = Map.insert v t (Local.solvedVars ctx)}
      ctx <- return $ ctx {Local.solvedVars = Map.map (Local.apply ctx) (Local.solvedVars ctx)}
      Right ctx

freshForallNames :: Id -> [Id]
freshForallNames x = x : [x ++ show n | n <- [2 ..]]
