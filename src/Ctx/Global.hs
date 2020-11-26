module Ctx.Global where

import qualified Data.Map as Map
import qualified Kind
import Lib (Id)
import qualified Type

data Ctx = Ctx
  { types :: Map.Map Id Kind.Kind,
    vals :: Map.Map Id Type.Type
  }

lookupConst :: Ctx -> Id -> Maybe Kind.Kind
lookupConst ctx x = Map.lookup x (types ctx)

lookupVal :: Ctx -> Id -> Maybe Type.Type
lookupVal ctx x = Map.lookup x (vals ctx)
