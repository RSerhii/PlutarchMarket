{-# LANGUAGE RankNTypes          #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Eval (
  evalConfig,
  evalWithArgs,
  evalWithArgsT,
  succeedsImpl,
  failsImpl,
  eraseRight,
  eraseLeft,
  eraseBoth,
  expectFailure,
  expectSuccess
) where

import Plutarch.Prelude
import PExtra.API
import Data.Text (Text, pack)
import Plutarch.Evaluate (evalScript, EvalError)
import Plutarch (ClosedTerm, compile, Config(..), TracingMode (..), Script (Script))
import PlutusLedgerApi.V1 (Data, ExBudget)
-- import PlutusLedgerApi.V1.Scripts (Script (unScript), applyArguments)
import Control.Arrow
import UntypedPlutusCore (DeBruijn, DefaultFun, DefaultUni, Program)
import PlutusTx (Data)
import Plutarch.Script (Script (unScript))
import Test.Tasty.HUnit ( Assertion, assertFailure )
-- import PlutusCore qualified as PLC
import PlutusCore.Data qualified as PLC
import PlutusCore.MkPlc qualified as PLC
import UntypedPlutusCore qualified as UPLC
import PlutusPrelude (over)
import Hedgehog
import Plutarch.Api.V1.Contexts (PScriptContext)
import Market.Utils
import Market.Contract

evalConfig :: Config
evalConfig = Config DoTracing

evalWithArgs :: ClosedTerm a -> [Data] -> Either Text (ExBudget, [Text], Program DeBruijn DefaultUni DefaultFun ())
evalWithArgs x args = do
  cmp <- compile evalConfig x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- left (pack . show) escr
  pure (budg, trc, unScript scr)

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Program DeBruijn DefaultUni DefaultFun ())
evalWithArgsT x args = do
  cmp <- compile evalConfig x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- left (pack . show) escr
  pure (unScript scr)

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

eraseRight :: Either a b -> Either a ()
eraseRight (Right _) = Right ()
eraseRight (Left l)  = Left l

eraseLeft :: Either a b -> Either () b
eraseLeft (Right l) = Right l
eraseLeft (Left _)  = Left ()

eraseBoth :: Either a b -> Either () ()
eraseBoth (Right _) = Right ()
eraseBoth (Left _)  = Left ()

expectFailure :: [Data] -> PropertyT IO ()
expectFailure args = do 
  let result = eraseBoth $  evalWithArgs (wrapValidator marketValidator) args
  result === Left ()

expectSuccess :: [Data] -> PropertyT IO ()
expectSuccess args = do 
  let result = eraseRight $  evalWithArgs (wrapValidator marketValidator) args
  result === Right ()
