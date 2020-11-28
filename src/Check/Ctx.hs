module Check.Ctx where

import Check.Error (Error)
import Control.Applicative ((<|>))
import qualified Ctx.Global as Global
import qualified Ctx.Local as Local
import qualified Data.Set as Set
import qualified Expr.Output as Output
import qualified Kind
import Lib (Id)
import qualified Type

lookupConst :: Global.Ctx -> Local.Ctx -> Id -> Either Error Kind.Kind
lookupConst gctx ctx x = case local <|> global of
  Just k -> Right k
  Nothing -> Left $ "Could not find type: " ++ x
  where
    local = snd <$> Local.lookupConst ctx x
    global = Global.lookupConst gctx x

lookupName :: Global.Ctx -> Local.Ctx -> Id -> Either Error Type.Type
lookupName gctx ctx x =
  case (Local.lookupName ctx x, Global.lookupName gctx x) of
    (Just k, _) -> Right k -- what type class is this?
    (_, Just k) -> Right k
    (Nothing, Nothing) -> Left $ "Can't find var: " ++ x

constLevel :: Global.Ctx -> Local.Ctx -> Id -> Either Error Local.Level
constLevel gctx ctx x = case (Local.lookupConst ctx x, Global.lookupConst gctx x) of
  (Just (l, _), _) -> Right l
  (Nothing, Just _) -> Right 0
  (Nothing, Nothing) -> Left $ "Could not find const: " ++ x

checkMono :: Local.Ctx -> Type.Type -> Either Error ()
checkMono ctx t =
  let t' = Local.apply ctx t
   in if Type.mono t'
        then return ()
        else Left $ "Expected a monotype, got: " ++ show t'

orderUsed :: Global.Ctx -> Local.Ctx -> Set.Set Id -> Either Error [Id]
orderUsed gctx ctx fs = go (map fst $ Local.names ctx) fs
  where
    go :: [Id] -> Set.Set Id -> Either Error [Id]
    go ns fs =
      if Set.null fs
        then Right []
        else case ns of
          [] -> case mapM (Global.lookupName gctx) (Set.toList fs) of
            Nothing -> Left $ "Vars not in ctx: " ++ show fs
            Just _ -> Right []
          n : ns | Set.member n fs -> do
            xs <- go ns (Set.delete n fs)
            Right (n : xs)
          _ : ns -> go ns fs

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
      (x : xs) -> case Local.lookupName ctx x of
        Nothing -> Left $ "Expected a local: " ++ x
        Just t -> do
          ts <- go ctx xs
          Right (t : ts)
