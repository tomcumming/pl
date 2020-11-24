module Check where

import Check.Error (Error)
import qualified Ctx
import qualified Data.Map as Map
import qualified Expr.Input as Input
import qualified Type
import Unify (unify)

infer ::
  Ctx.Ctx ->
  Input.Expr ->
  Either Error (Ctx.Ctx, Type.Type)
infer ctx e = case e of
  Input.Var x -> case Map.lookup x (Ctx.named ctx) of
    Nothing -> Left $ "Can't find variable: " ++ x
    Just t -> Right (ctx, t)
  Input.Ap eFn eArg -> do
    (ctx, tFn) <- infer ctx eFn
    -- TODO Apply type args
    case Ctx.apply ctx tFn of
      Type.Ap (Type.Ap (Type.Ap (Type.Const "Fn") _) tArg) tRet -> do
        ctx <- check ctx eArg tArg
        Right (ctx, tRet)
      Type.Var c -> error "TODO infer application for var"
      t -> Left $ show t ++ " is not a function"

check ::
  Ctx.Ctx ->
  Input.Expr ->
  Type.Type ->
  Either Error Ctx.Ctx
check ctx e t = case (e, t) of
  (e, Type.Forall x k t) -> error "TODO check forall"
  -- TODO (Abs, Arrow)
  (e, t) -> do
    (ctx, t2) <- infer ctx e
    unify ctx t t2
