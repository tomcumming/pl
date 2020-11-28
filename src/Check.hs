module Check where

import qualified Check.Ctx
import Check.Error (Error)
import qualified Ctx.Global as Global
import qualified Ctx.Local as Local
import qualified Data.Set as Set
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
    t <- Check.Ctx.lookupName gctx ctx x
    return (ctx, Output.Var x, t)
  Input.Ap eFn eArg -> do
    (ctx, eFn, tFn) <- infer gctx ctx eFn
    (ctx, eFn, tFn) <- return $ applyTypeArgs ctx eFn tFn
    case Local.apply ctx tFn of
      Type.Ap (Type.Ap (Type.Ap Type.Fn _) tArg) tRet -> do
        (ctx, eArg) <- check gctx ctx eArg tArg
        Right (ctx, Output.Ap eFn eArg, tRet)
      Type.Var vFn -> do
        (ctx, eArg, tArg) <- infer gctx ctx eArg
        (ctx, vCls) <- return $ Local.addVar ctx Kind.Star
        (ctx, vRet) <- return $ Local.addVar ctx Kind.Star
        ctx <- unify gctx ctx (Type.Var vFn) (Type.fn (Type.Var vCls) tArg (Type.Var vRet))
        return (ctx, Output.Ap eFn eArg, Type.Var vRet)
      t -> Left $ show t ++ " is not a function"
  Input.Abs x e -> do
    (ctx, vArg) <- return $ Local.addVar ctx Kind.Star
    ctx <- return $ Local.addName ctx x (Type.Var vArg)
    (ctx, e, tRet) <- infer gctx ctx e
    cNames <- Check.Ctx.orderUsed gctx ctx (Set.delete x (Output.usedVars e))
    cs <- Check.Ctx.closureTypes ctx cNames
    ctx <- return $ Local.popName ctx
    Right (ctx, Output.Abs x (Type.Var vArg) e cNames, Type.fn cs (Type.Var vArg) tRet)

check ::
  Global.Ctx ->
  Local.Ctx ->
  Input.Expr ->
  Type.Type ->
  Either Error (Local.Ctx, Output.Expr)
check gctx ctx e t = case (e, Local.apply ctx t) of
  (e, Type.Forall x k t) -> do
    (ctx, e) <- check gctx (Local.addConst ctx x k) e t
    Right (Local.popConst ctx, Output.TypeAbs x k e)
  (Input.Abs x e, Type.Ap (Type.Ap (Type.Ap Type.Fn tCls) tArg) tRet) -> do
    (ctx, e) <- check gctx (Local.addName ctx x tArg) e tRet
    cNames <- Check.Ctx.orderUsed gctx ctx (Set.delete x (Output.usedVars e))
    cs <- Check.Ctx.closureTypes ctx cNames
    ctx <- unify gctx ctx tCls cs
    ctx <- return $ Local.popName ctx
    Right (ctx, Output.Abs x tArg e cNames)
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
  Type.Forall x k t -> applyTypeArgs ctx' e' t'
    where
      (ctx', v) = Local.addVar ctx k
      e' = Output.TypeAp e (Type.Var v)
      t' = Type.subsConst x t (Type.Var v)
  t -> (ctx, e, t)
