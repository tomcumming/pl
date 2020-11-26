module Check.Ctx where

import Check.Error (Error)
import qualified Ctx.Global as Global
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

lookupConst :: Global.Ctx -> Local.Ctx -> Id -> Either Error Kind.Kind
lookupConst gctx ctx x =
  case (Local.lookupConst ctx x, Global.lookupConst gctx x) of
    (Just k, _) -> Right k -- what type class is this?
    (_, Just k) -> Right k
    (Nothing, Nothing) -> Left $ "Can't find var: " ++ x

lookupVal :: Global.Ctx -> Local.Ctx -> Id -> Either Error Type.Type
lookupVal gctx ctx x =
  case (Local.lookupVal ctx x, Global.lookupVal gctx x) of
    (Just k, _) -> Right k -- what type class is this?
    (_, Just k) -> Right k
    (Nothing, Nothing) -> Left $ "Can't find var: " ++ x

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

orderUsed :: Global.Ctx -> Local.Ctx -> Set.Set Id -> Either Error [Id]
orderUsed gctx ctx xs = go (Local.parts ctx) xs
  where
    go ctx xs =
      if Set.null xs
        then Right []
        else case ctx of
          [] -> case mapM (Global.lookupVal gctx) (Set.toList xs) of
            Nothing -> Left $ "Vars not in ctx: " ++ show xs
            Just _ -> Right []
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
      (x : xs) -> case Local.lookupVal ctx x of
        Nothing -> Left $ "Expected a local: " ++ x
        Just t -> do
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
