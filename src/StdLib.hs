module StdLib (ctx) where

import qualified Ctx.Global as Global
import qualified Data.Map as Map
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

ctx :: Global.Ctx
ctx =
  Global.Ctx
    { Global.names =
        Map.fromList
          [ ("Unit", Type.Const "Unit"),
            ("True", Type.Const "Bool"),
            ("False", Type.Const "Bool"),
            ("Just", justType),
            ("None", noneType)
          ],
      Global.types =
        Map.fromList
          [ ("Unit", Kind.Star),
            ("Bool", Kind.Star),
            ("Pair", Kind.Arrow Kind.Star (Kind.Arrow Kind.Star Kind.Star)),
            ("Either", Kind.Arrow Kind.Star (Kind.Arrow Kind.Star Kind.Star)),
            ("Maybe", Kind.Arrow Kind.Star Kind.Star)
          ]
    }
