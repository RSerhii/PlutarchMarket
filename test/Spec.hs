module Spec (spec) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified PlutusTx
import Market.Contract (alwaysSucceeds, alwaysFails)
import Eval (evalWithArgsT, succeedsImpl, failsImpl)

spec :: TestTree
spec =
  testGroup
    "Validator tests"
    [
      alwaysSucceedsTests
    ]

alwaysSucceedsTests :: TestTree
alwaysSucceedsTests = do
  testGroup
    "alwaysSucceedsTests"
    [ testCase "run alwaysSucceedsTests" $ do
        succeedsImpl $ alwaysSucceeds `evalWithArgsT` [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()]
      , testCase "run alwaysFails" $ do
        failsImpl $ alwaysFails `evalWithArgsT` [PlutusTx.toData (), PlutusTx.toData (), PlutusTx.toData ()]
    ]
