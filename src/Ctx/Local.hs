module Ctx.Local where

import qualified Data.Map as Map
import qualified Kind
import Lib (Id)
import qualified Type

type Level = Word

data Ctx = Ctx
  { freshVar :: Type.Var,
    typeNames :: [(String, Kind.Kind)],
    names :: [(String, Type.Type)],
    vars :: Map.Map Type.Var (Level, Kind.Kind),
    solvedVars :: Map.Map Type.Var Type.Type
    -- TODO instances & solved instances
  }
  deriving (Show)

empty :: Ctx
empty =
  Ctx
    { freshVar = 0,
      typeNames = [],
      names = [],
      vars = Map.empty,
      solvedVars = Map.empty
    }

lookupVar :: Ctx -> Type.Var -> (Level, Kind.Kind, Maybe Type.Type)
lookupVar ctx v = case (Map.lookup v (vars ctx), Map.lookup v (solvedVars ctx)) of
  (Nothing, _) -> error $ "Could not lookup var " ++ show v
  (Just (l, k), t) -> (l, k, t)

lookupConst :: Ctx -> Id -> Maybe (Level, Kind.Kind)
lookupConst ctx x = go (typeNames ctx)
  where
    go ns = case ns of
      [] -> Nothing
      (n, k) : ns | n == x -> Just (fromIntegral (1 + length ns), k)
      _ : ns -> go ns

lookupName :: Ctx -> Id -> Maybe Type.Type
lookupName ctx x = go (names ctx)
  where
    go ns = case ns of
      [] -> Nothing
      (n, t) : _ | n == x -> Just t
      _ : ns -> go ns

apply :: Ctx -> Type.Type -> Type.Type
apply ctx t = case t of
  Type.Var v -> case Map.lookup v (solvedVars ctx) of
    Nothing -> Type.Var v
    Just t -> t
  Type.Const x -> Type.Const x
  Type.Fn -> Type.Fn
  Type.Ap t t2 -> Type.Ap (apply ctx t) (apply ctx t2)
  Type.Forall x k t -> Type.Forall x k (apply ctx t)

addVar :: Ctx -> Kind.Kind -> (Ctx, Type.Var)
addVar ctx k =
  ( ctx
      { freshVar = freshVar ctx + 1,
        vars =
          Map.insert
            (freshVar ctx)
            (fromIntegral $ length (typeNames ctx), k)
            (vars ctx)
      },
    freshVar ctx
  )

addName :: Ctx -> Id -> Type.Type -> Ctx
addName ctx x k = ctx {names = (x, k) : names ctx}

addConst :: Ctx -> Id -> Kind.Kind -> Ctx
addConst ctx x k = ctx {typeNames = (x, k) : typeNames ctx}

popName :: Ctx -> Ctx
popName ctx = ctx {names = tail (names ctx)}

popConst :: Ctx -> Ctx
popConst ctx = ctx {typeNames = tail (typeNames ctx)}
