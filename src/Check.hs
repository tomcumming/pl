module Check where

import qualified Check.Ctx
import Check.Error (Error)
import qualified Ctx.Local as Local
import qualified Expr.Input as Input
import qualified Type
import Unify (unify)

infer ::
  Local.Ctx ->
  Input.Expr ->
  Either Error (Local.Ctx, Type.Type)
infer ctx e = case e of
  Input.Var x -> do
    t <- Check.Ctx.lookupVal ctx x
    return (ctx, t)
  Input.Ap eFn eArg -> do
    (ctx, tFn) <- infer ctx eFn
    -- TODO Apply type args
    case Local.apply ctx tFn of
      Type.Ap (Type.Ap (Type.Ap (Type.Const "Fn") _) tArg) tRet -> do
        ctx <- check ctx eArg tArg
        Right (ctx, tRet)
      Type.Var c -> error "TODO infer application for var"
      t -> Left $ show t ++ " is not a function"

check ::
  Local.Ctx ->
  Input.Expr ->
  Type.Type ->
  Either Error Local.Ctx
check ctx e t = case (e, t) of
  (e, Type.Forall x k t) -> error "TODO check forall"
  -- TODO (Abs, Arrow)
  (e, t) -> do
    (ctx, t2) <- infer ctx e
    unify ctx t t2
