module Check.ForallEscapeSpec (spec) where

import Check (infer)
import qualified Ctx.Global as Global
import qualified Ctx.Local as Local
import Data.Either (isRight)
import qualified Data.Map as Map
import qualified Expr.Input as Input
import qualified Kind
import qualified StdLib
import Test.Hspec
import qualified Type

spec :: Spec
spec = describe "Test naive forall escape" $ do
  let escapeArg = Type.Forall "a" Kind.Star $ Type.fn (Type.Const "Unit") (Type.Const "a") (Type.Const "b")
  let escape = Type.Forall "b" Kind.Star $ Type.fn (Type.Const "Unit") escapeArg (Type.Const "b")

  let ctx = StdLib.ctx {Global.names = Map.insert "escape" escape (Global.names StdLib.ctx)}

  it "Allows non escape" $ do
    let result = infer ctx Local.empty $ Input.Ap (Input.Var "escape") (Input.Abs "x" (Input.Var "True"))
    case result of
      Left e -> expectationFailure e
      Right (ctx, _, t) -> Local.apply ctx t `shouldBe` Type.Const "Bool"

  it "Catches id escape" $ do
    let result = infer ctx Local.empty $ Input.Ap (Input.Var "escape") (Input.Abs "x" (Input.Var "True"))
    case result of
      Left e -> return ()
      Right (ctx, _, t) -> expectationFailure $ "Inferred type: " ++ show (Local.apply ctx t)
