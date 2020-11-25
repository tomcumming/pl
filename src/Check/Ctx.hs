module Check.Ctx where

import Check.Error (Error)
import qualified Ctx.Local as Local
import qualified Kind
import Lib (Id)
import qualified Type

lookupVar :: Local.Ctx -> Type.Var -> Either Error Local.TypeVar
lookupVar ctx v = case Local.lookupVar ctx v of
  Nothing -> Left $ "Can't find type var: " ++ show v
  Just tv -> Right tv

lookupConst :: Local.Ctx -> Id -> Either Error Kind.Kind
lookupConst ctx x = case Local.lookupConst ctx x of
  Nothing -> Left $ "Can't find var: " ++ x
  Just k -> Right k

lookupVal :: Local.Ctx -> Id -> Either Error Type.Type
lookupVal ctx x = case Local.lookupVal ctx x of
  Nothing -> Left $ "Can't find var: " ++ x
  Just t -> Right t

orderVars :: Local.Ctx -> Type.Var -> Type.Var -> Either Error (Type.Var, Type.Var)
orderVars ctx v u = case (Local.varIndex ctx v, Local.varIndex ctx u) of
  (Just vi, Just ui) -> Right $ if vi > ui then (ui, vi) else (vi, ui)
  _ -> Left $ unwords ["Could not find", show v, "or", show u]

checkMono :: Local.Ctx -> Type.Type -> Either Error ()
checkMono ctx t =
  if Type.mono (Local.apply ctx t)
    then return ()
    else Left $ "Expected a monotype, got: " ++ show t
