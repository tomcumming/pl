module Check.Kind where

import Check.Error (Error)
import qualified Ctx
import qualified Data.Map as Map
import qualified Kind
import qualified Type

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
      _ -> Left $ unwords ["Type application:", show c, show a]
  Type.Forall {} -> error "TODO kind of a forall"
