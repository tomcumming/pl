module Check where

import qualified Ctx
import qualified Data.Map as Map
import qualified Expr.Input as Input
import qualified Kind
import qualified Type

type Error = String

kind ::
  Ctx.Ctx ->
  Type.Type ->
  Either Error Kind.Kind
kind ctx t = case t of
  Type.Var v -> case Map.lookup v (Ctx.typeVars ctx) of
    Just (_, k) -> Right k
    Nothing -> Left $ "Unknown type var: " ++ show v
  Type.Const x -> case Map.lookup x (Ctx.typeConsts ctx) of
    Just k -> Right k
    Nothing -> Left $ "Unknown type const: " ++ x
  Type.Fn -> Right Kind.fnKind
  Type.Ap c a -> do
    ck <- kind ctx c
    ak <- kind ctx a
    case ck of
      Kind.Arrow cka ckr | cka == ak -> Right ckr
      _ -> Left $ concat ["Type application:", show c, show a]
  Type.Forall {} -> error "TODO kind of a forall"

infer ::
  Ctx.Ctx ->
  Input.Expr ->
  Either Error (Ctx.Ctx, Type.Type)
infer ctx e = case e of
  Input.Var x -> case Map.lookup x (Ctx.named x) of
    Nothing -> Left $ "Can't find variable: " ++ x
    Just t -> Right (ctx, t)
  Input.Ap eFn eArg -> do
    (ctx, tFn) <- infer ctx eFn
    -- TODO Apply type args
    case Ctx.apply ctx tFn of
      Type.Ap (Type.Ap (Type.Ap Type.Fn _) tArg) tRet -> do
        ctx <- check ctx eArg tArg
        Right (ctx, tRet)
      Type.Var c -> error "TODO infer application for var"
      t -> Left $ show t ++ " is not a function"

check ::
  Ctx.Ctx ->
  Input.Expr ->
  Type.Type ->
  Either Error Ctx.Ctx
check = undefined
