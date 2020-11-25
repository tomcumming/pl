module Ctx.Local where

import qualified Kind
import Lib (Id)
import qualified Type

data Ctx = Ctx
  { parts :: [CtxPart],
    freshVar :: Type.Var
  }
  deriving (Show)

data TypeVar
  = Unsolved Kind.Kind
  | Solved Type.Type -- Must be a monotype
  deriving (Show)

data CtxPart
  = TypeVar Type.Var TypeVar
  | Const Id Kind.Kind
  | Val Id Type.Type
  deriving (Show) -- might need a marker ?

fresh :: Ctx -> (Ctx, Type.Var)
fresh ctx = (ctx {freshVar = succ (freshVar ctx)}, freshVar ctx)

lookupVar :: Ctx -> Type.Var -> Maybe TypeVar
lookupVar ctx v = go (parts ctx)
  where
    go :: [CtxPart] -> Maybe TypeVar
    go ctx = case ctx of
      [] -> Nothing
      TypeVar v2 tv : _ | v == v2 -> Just tv
      _ : ctx -> go ctx

lookupConst :: Ctx -> Id -> Maybe Kind.Kind
lookupConst ctx x = go (parts ctx)
  where
    go :: [CtxPart] -> Maybe Kind.Kind
    go ctx = case ctx of
      [] -> Nothing
      Const y k : _ | x == y -> Just k
      _ : ctx -> go ctx

lookupVal :: Ctx -> Id -> Maybe Type.Type
lookupVal ctx x = go (parts ctx)
  where
    go :: [CtxPart] -> Maybe Type.Type
    go ctx = case ctx of
      [] -> Nothing
      Val y t : _ | x == y -> Just t
      _ : ctx -> go ctx

apply :: Ctx -> Type.Type -> Type.Type
apply ctx t = case t of
  Type.Var v -> case lookupVar ctx v of
    Just (Solved t) -> t
    _ -> Type.Var v
  Type.Const x -> Type.Const x
  Type.Fn -> Type.Fn
  Type.Ap t t2 -> Type.Ap (apply ctx t) (apply ctx t2)
  Type.Forall x k t -> Type.Forall x k (apply ctx t)

varIndex :: Ctx -> Type.Var -> Maybe Word
varIndex ctx v = go (parts ctx) 0
  where
    go ctx n = case ctx of
      [] -> Nothing
      TypeVar u _ : _ | v == u -> Just n
      _ : ctx -> go ctx (succ n)

insertBehind :: Ctx -> Type.Var -> [CtxPart] -> Ctx
insertBehind ctx v ps = go [] (parts ctx)
  where
    go ctxHead ctxTail = case ctxTail of
      TypeVar u tv : ctxTail
        | v == u ->
          let parts = reverse ctxHead ++ TypeVar u tv : ps ++ ctxTail
           in ctx {parts = parts}
      p : ctxTail -> go (p : ctxHead) ctxTail

push :: CtxPart -> Ctx -> Ctx
push p ctx = ctx {parts = p : parts ctx}
