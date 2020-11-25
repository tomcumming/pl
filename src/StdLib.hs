module StdLib (ctx) where

import qualified Ctx.Local as Local
import qualified Kind
import qualified Type

justType :: Type.Type
justType =
  Type.Forall "a" Kind.Star $
    Type.fn
      (Type.Const "Unit")
      (Type.Const "a")
      (Type.Ap (Type.Const "Maybe") (Type.Const "a"))

noneType :: Type.Type
noneType = Type.Forall "a" Kind.Star (Type.Ap (Type.Const "Maybe") (Type.Const "a"))

ctx :: Local.Ctx
ctx =
  Local.Ctx
    { Local.freshVar = 0,
      Local.parts =
        [ Local.Val "Unit" $ Type.Const "Unit",
          Local.Val "Just" justType,
          Local.Val "None" noneType,
          Local.Const "Unit" Kind.Star,
          Local.Const "Bool" Kind.Star,
          Local.Const "Pair" $ Kind.Arrow Kind.Star (Kind.Arrow Kind.Star Kind.Star),
          Local.Const "Either" $ Kind.Arrow Kind.Star (Kind.Arrow Kind.Star Kind.Star),
          Local.Const "Maybe" $ Kind.Arrow Kind.Star Kind.Star
        ]
    }
