module Check.Kind where

import qualified Check.Ctx
import Check.Error (Error)
import qualified Ctx.Global as Global
import qualified Ctx.Local as Local
import qualified Kind
import qualified Type

kind ::
  Global.Ctx ->
  Local.Ctx ->
  Type.Type ->
  Either Error Kind.Kind
kind gctx ctx t = case t of
  Type.Var v -> do
    tv <- Check.Ctx.lookupVar ctx v
    case tv of
      Local.Unsolved k -> Right k
      Local.Solved t -> kind gctx ctx t
  Type.Const x -> Check.Ctx.lookupConst gctx ctx x
  Type.Fn -> Right Kind.fnKind
  Type.Ap c a -> do
    ck <- kind gctx ctx c
    ak <- kind gctx ctx a
    case ck of
      Kind.Arrow cka ckr | cka == ak -> Right ckr
      _ -> Left $ unwords ["Type application:", show c, show a]
  Type.Forall {} -> error "TODO kind of a forall"
