module Check.ApToTypeVarSpec (spec) where

import Check (infer)
import qualified Ctx.Local as Local
import qualified Expr.Input as Input
import qualified Kind
import qualified StdLib
import Test.Hspec
import qualified Type

spec :: Spec
spec = describe "Test functions are recognised via application" $ do
  it "Should allow application to an unknown" $ do
    let (ctx, vFn) = Local.addVar Local.empty Kind.Star
    ctx <- return $ Local.addName ctx "f" (Type.Var vFn)
    let expr = Input.Ap (Input.Var "f") (Input.Var "True")
    case infer StdLib.ctx ctx expr of
      Left e -> expectationFailure e
      Right (ctx, _, _) -> case Local.apply ctx (Type.Var vFn) of
        Type.Ap (Type.Ap (Type.Ap Type.Fn _) (Type.Const "Bool")) _ -> return ()
        t -> expectationFailure $ "Expected a function with input 'Bool':" ++ show t
