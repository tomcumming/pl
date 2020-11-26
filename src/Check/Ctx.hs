module Check.Ctx where

import Check.Error (Error)
import qualified Ctx.Local as Local
import qualified Data.Set as Set
import qualified Expr.Output as Output
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
checkMono ctx t = do
  t <- apply ctx t
  if Type.mono t
    then return ()
    else Left $ "Expected a monotype, got: " ++ show t

orderUsed :: Local.Ctx -> Set.Set Id -> Either Error [Id]
orderUsed ctx xs = go (Local.parts ctx) xs
  where
    go ctx xs =
      if Set.null xs
        then Right []
        else case ctx of
          [] -> Left $ "Vars not in ctx: " ++ show xs
          Local.Val x _ : ctx | Set.member x xs -> do
            xs <- go ctx (Set.delete x xs)
            Right (x : xs)
          _ : ctx -> go ctx xs

closureType :: [Type.Type] -> Type.Type
closureType ts = case ts of
  [] -> Type.Const "Unit"
  [t] -> t
  t : ts -> Type.Ap (Type.Ap (Type.Const "Pair") t) (closureType ts)

closureTypes :: Local.Ctx -> [Id] -> Either Error Type.Type
closureTypes ctx xs = closureType <$> go ctx xs
  where
    go ctx xs = case xs of
      [] -> Right []
      (x : xs) -> do
        t <- lookupVal ctx x
        ts <- go ctx xs
        Right (t : ts)

apply :: Local.Ctx -> Type.Type -> Either Error Type.Type
apply ctx t = case Local.apply ctx t of
  Just t -> Right t
  Nothing -> Left $ "Missing type variable in context during substitution"

applyToExpr :: Local.Ctx -> Output.Expr -> Either Error Output.Expr
applyToExpr ctx e = case e of
  Output.Var x -> Right $ Output.Var x
  Output.Ap e1 e2 -> do
    e1 <- applyToExpr ctx e1
    e2 <- applyToExpr ctx e2
    Right $ Output.Ap e1 e2
  Output.Abs x t e cs -> do
    t <- apply ctx t
    e <- applyToExpr ctx e
    Right $ Output.Abs x t e cs
  Output.TypeAp e t -> do
    t <- apply ctx t
    e <- applyToExpr ctx e
    Right $ Output.TypeAp e t
  Output.TypeAbs x k e -> Output.TypeAbs x k <$> applyToExpr ctx e
