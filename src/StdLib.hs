module StdLib where

import qualified Ctx
import qualified Data.Map as Map
import qualified Kind
import qualified Type

ctx :: Ctx.Ctx
ctx =
  Ctx.Ctx
    { Ctx.typeConsts =
        Map.fromList
          [ ("Unit", Kind.Star),
            ("Bool", Kind.Star),
            ("Pair", Kind.Arrow Kind.Star (Kind.Arrow Kind.Star Kind.Star)),
            ("Either", Kind.Arrow Kind.Star (Kind.Arrow Kind.Star Kind.Star)),
            ("Maybe", Kind.Arrow Kind.Star Kind.Star)
          ],
      Ctx.typeVars = Map.empty,
      Ctx.typeSubs = Map.empty,
      Ctx.named =
        Map.fromList
          [ ("Unit", Type.Const "Unit"),
            ("True", Type.Const "Bool"),
            ("False", Type.Const "Bool"),
            ("None", Type.Const "Maybe"),
            ( "Some",
              Type.Forall
                "a"
                Kind.Star
                (Type.fn (Type.Const "Unit") (Type.Const "a") (Type.Ap (Type.Const "Maybe") (Type.Const "a")))
            )
          ],
      Ctx.level = 0
    }
