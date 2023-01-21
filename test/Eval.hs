{-# OPTIONS_GHC -Wno-unused-matches #-}
module Eval (evalT, evalWithArgsT, evalWithArgsT', succeedsImpl, failsImpl) where

import Data.Bifunctor (first)
import Data.Default (def)
import Data.Text (Text, pack)
import Plutarch (ClosedTerm, compile, Script (Script))
import Plutarch.Evaluate (evalScript)
import PlutusLedgerApi.V1 (Data, ExBudget)
import Plutarch.Script (Script (unScript))
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)
import Test.Tasty.HUnit
-- import PlutusCore qualified as PLC
import PlutusCore.Data qualified as PLC
import PlutusCore.MkPlc qualified as PLC
import UntypedPlutusCore qualified as UPLC
import PlutusPrelude (over)

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile def x
--   let (escr, budg, trc) = evalScript cmp
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

evalWithArgsT' :: ClosedTerm a -> [Data] -> Either Text (Program DeBruijn DefaultUni DefaultFun (), ExBudget, [Text])
evalWithArgsT' x args =
  (\(res, budg, trcs) -> (unScript res, budg, trcs))
    <$> evalWithArgsT x args


succeedsImpl :: Either Text (Script, ExBudget, [Text]) -> Assertion
succeedsImpl x = case x of
    Left e -> assertFailure $ "Script evaluation failed: " <> show e
    Right _ -> pure ()

failsImpl :: Either Text (Script, ExBudget, [Text]) -> Assertion
failsImpl x =
    case x of
    Left _ -> pure ()
    Right (s, _, _) -> assertFailure $ "Script didn't err: " <> show s

applyArguments :: Script -> [PLC.Data] -> Script
applyArguments (Script p) args =
    let termArgs = fmap (PLC.mkConstant ()) args
        applied t = PLC.mkIterApp () t termArgs
    in Script $ over UPLC.progTerm applied p
