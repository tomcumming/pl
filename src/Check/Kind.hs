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
  Type.Var v -> case Local.lookupVar ctx v of
    (_, k, _) -> Right k
  Type.Const x -> Check.Ctx.lookupConst gctx ctx x
  Type.Fn -> Right Kind.fnKind
  Type.Ap c a -> do
    ck <- kind gctx ctx c
    ak <- kind gctx ctx a
    case ck of
      Kind.Arrow cka ckr | cka == ak -> Right ckr
      _ -> Left $ unwords ["Type application:", show c, show a]
  Type.Forall {} -> error "TODO kind of a forall"
