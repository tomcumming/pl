module Check where

import qualified Check.Ctx
import Check.Error (Error)
import qualified Ctx.Local as Local
import qualified Expr.Input as Input
import qualified Expr.Output as Output
import qualified Type
import Unify (unify)

infer ::
  Local.Ctx ->
  Input.Expr ->
  Either Error (Local.Ctx, Output.Expr, Type.Type)
infer ctx e = case e of
  Input.Var x -> do
    t <- Check.Ctx.lookupVal ctx x
    return (ctx, Output.Var x, t)
  Input.Ap eFn eArg -> do
    (ctx, eFn, tFn) <- infer ctx eFn
    (ctx, eFn, tFn) <- return $ applyTypeArgs ctx eFn tFn
    tFn <- Check.Ctx.apply ctx tFn
    case tFn of
      Type.Ap (Type.Ap (Type.Ap Type.Fn _) tArg) tRet -> do
        (ctx, eArg) <- check ctx eArg tArg
        Right (ctx, Output.Ap eFn eArg, tRet)
      Type.Var c -> error "TODO infer application for var"
      t -> Left $ show t ++ " is not a function"

check ::
  Local.Ctx ->
  Input.Expr ->
  Type.Type ->
  Either Error (Local.Ctx, Output.Expr)
check ctx e t = case (e, t) of
  (e, Type.Forall x k t) -> do
    (ctx, e) <- check (Local.push (Local.Const x k) ctx) e t
    e <- Check.Ctx.applyToExpr ctx e
    case Local.splitOnConst ctx x of
      Nothing -> Left $ "Could not find in ctx: " ++ x
      Just ctx -> do
        -- do we need to check e is valid in ctx now?
        Right (ctx, Output.TypeAbs x k e)
  (Input.Abs x e, Type.Ap (Type.Ap (Type.Ap Type.Fn tCtx) tArg) tRet) -> do
    (ctx, e) <- check (Local.push (Local.Val x tArg) ctx) e tRet
    e <- Check.Ctx.applyToExpr ctx e
    case Local.splitOnVal ctx x of
      Nothing -> Left $ "Could not find in ctx: " ++ x
      Just ctx -> do
        cNames <- Check.Ctx.orderUsed ctx (Output.usedVars e)
        cs <- Check.Ctx.closureTypes ctx cNames
        ctx <- unify ctx tCtx cs
        -- check if e valid in ctx?
        Right (ctx, e)
  (e, t) -> do
    (ctx, e, t2) <- infer ctx e
    (ctx, e, t2) <- return $ applyTypeArgs ctx e t2
    ctx <- unify ctx t t2
    Right (ctx, e)

applyTypeArgs ::
  Local.Ctx ->
  Output.Expr ->
  Type.Type ->
  (Local.Ctx, Output.Expr, Type.Type)
applyTypeArgs ctx e t = case t of
  Type.Forall x k t -> applyTypeArgs ctx'' e' t'
    where
      (ctx', v) = Local.fresh ctx
      ctx'' = Local.push (Local.TypeVar v (Local.Unsolved k)) ctx'
      e' = Output.TypeAp e (Type.Var v)
      t' = Type.subsConst x t (Type.Var v)
  t -> (ctx, e, t)
