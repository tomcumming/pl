module Check.Kind where

import qualified Check.Ctx
import Check.Error (Error)
import qualified Ctx.Local as Local
import qualified Kind
import qualified Type

kind ::
  Local.Ctx ->
  Type.Type ->
  Either Error Kind.Kind
kind ctx t = case t of
  Type.Var v -> do
    tv <- Check.Ctx.lookupVar ctx v
    case tv of
      Local.Unsolved k -> Right k
      Local.Solved t -> kind ctx t
  Type.Const x -> Check.Ctx.lookupConst ctx x
  Type.Fn -> Right Kind.fnKind
  Type.Ap c a -> do
    ck <- kind ctx c
    ak <- kind ctx a
    case ck of
      Kind.Arrow cka ckr | cka == ak -> Right ckr
      _ -> Left $ unwords ["Type application:", show c, show a]
  Type.Forall {} -> error "TODO kind of a forall"
