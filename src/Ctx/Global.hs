module Ctx.Global where

import qualified Data.Map as Map
import qualified Kind
import Lib (Id)
import qualified Type

data Ctx = Ctx
  { types :: Map.Map Id Kind.Kind,
    names :: Map.Map Id Type.Type
  }

lookupConst :: Ctx -> Id -> Maybe Kind.Kind
lookupConst ctx x = Map.lookup x (types ctx)

lookupName :: Ctx -> Id -> Maybe Type.Type
lookupName ctx x = Map.lookup x (names ctx)
