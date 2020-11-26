module Check where

import qualified Check.Ctx
import Check.Error (Error)
import qualified Ctx.Global as Global
import qualified Ctx.Local as Local
import qualified Expr.Input as Input
import qualified Expr.Output as Output
import qualified Kind
import qualified Type
import Unify (unify)

infer ::
  Global.Ctx ->
  Local.Ctx ->
  Input.Expr ->
  Either Error (Local.Ctx, Output.Expr, Type.Type)
infer gctx ctx e = case e of
  Input.Var x -> do
    t <- Check.Ctx.lookupVal gctx ctx x
    return (ctx, Output.Var x, t)
  Input.Ap eFn eArg -> do
    (ctx, eFn, tFn) <- infer gctx ctx eFn
    (ctx, eFn, tFn) <- return $ applyTypeArgs ctx eFn tFn
    tFn <- Check.Ctx.apply ctx tFn
    case tFn of
      Type.Ap (Type.Ap (Type.Ap Type.Fn _) tArg) tRet -> do
        (ctx, eArg) <- check gctx ctx eArg tArg
        Right (ctx, Output.Ap eFn eArg, tRet)
      Type.Var _ -> error "TODO Do we ever hit this?"
      t -> Left $ show t ++ " is not a function"
  Input.Abs x e -> do
    (ctx, vArg) <- return $ Local.fresh ctx
    ctx <- return $ Local.push (Local.TypeVar vArg (Local.Unsolved Kind.Star)) ctx
    ctx <- return $ Local.push (Local.Val x (Type.Var vArg)) ctx
    (ctx, e, tRet) <- infer gctx ctx e
    e <- Check.Ctx.applyToExpr ctx e
    tRet <- Check.Ctx.apply ctx tRet
    case Local.splitOnVal ctx x of
      Nothing -> Left $ "Expected to find in ctx: " ++ x
      Just ctx -> do
        cNames <- Check.Ctx.orderUsed gctx ctx (Output.usedVars e)
        cs <- Check.Ctx.closureTypes ctx cNames
        -- check if e valid in ctx?
        Right (ctx, e, Type.fn cs (Type.Var vArg) tRet)

check ::
  Global.Ctx ->
  Local.Ctx ->
  Input.Expr ->
  Type.Type ->
  Either Error (Local.Ctx, Output.Expr)
check gctx ctx e t = case (e, t) of
  (e, Type.Forall x k t) -> do
    (ctx, e) <- check gctx (Local.push (Local.Const x k) ctx) e t
    e <- Check.Ctx.applyToExpr ctx e
    case Local.splitOnConst ctx x of
      Nothing -> Left $ "Could not find in ctx: " ++ x
      Just ctx -> do
        -- do we need to check e is valid in ctx now?
        Right (ctx, Output.TypeAbs x k e)
  (Input.Abs x e, Type.Ap (Type.Ap (Type.Ap Type.Fn tCtx) tArg) tRet) -> do
    (ctx, e) <- check gctx (Local.push (Local.Val x tArg) ctx) e tRet
    e <- Check.Ctx.applyToExpr ctx e
    tCtx <- Check.Ctx.apply ctx tCtx
    case Local.splitOnVal ctx x of
      Nothing -> Left $ "Could not find in ctx: " ++ x
      Just ctx -> do
        cNames <- Check.Ctx.orderUsed gctx ctx (Output.usedVars e)
        cs <- Check.Ctx.closureTypes ctx cNames
        ctx <- unify gctx ctx tCtx cs
        -- check if e valid in ctx?
        Right (ctx, e)
  (e, t) -> do
    (ctx, e, t2) <- infer gctx ctx e
    (ctx, e, t2) <- return $ applyTypeArgs ctx e t2
    ctx <- unify gctx ctx t t2
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
