module Spec (spec) where

import Test.Tasty
import Test.Tasty.Hedgehog as HH
import Hedgehog
import qualified PlutusTx
import Eval (expectFailure, expectSuccess)
import Market.Types
import TestUtils

spec :: TestTree
spec =
  testGroup
    "marketValidator tests"
    [
      cancelOfferTestGroup
    ]

cancelOfferTestGroup :: TestTree
cancelOfferTestGroup = testGroup "CancelOffer"
  [ 
    HH.testProperty "cancelOfferWithoutContext" cancelOfferWithoutContext,
    HH.testProperty "cancelOfferWithoutDatum" cancelOfferWithoutDatum,
    HH.testProperty "cancelOfferSuccess" cancelOfferSuccess,
    HH.testProperty "cancelOfferWithWrongSigner" cancelOfferWithWrongSigner
  ]


cancelOfferWithoutContext :: Property
cancelOfferWithoutContext = property $ do
  seller <- forAll genPkh
  let 
    (token, _) = genAssets
    offerDatum = genDatum token seller 10
    offerRedeemer = mkOfferData CancelOffer
  expectFailure [offerDatum, offerRedeemer, PlutusTx.toData ()]

cancelOfferWithoutDatum :: Property
cancelOfferWithoutDatum = property $ do
  seller <- forAll genPkh
  orderTxRef <- forAll genTxOutRef
  let 
    offerRedeemer = mkOfferData CancelOffer
    txInfo = mkTxInfo seller
    purpose = mkPurpose orderTxRef
    cxtToData = PlutusTx.toData $ mkContext txInfo purpose
  expectFailure [PlutusTx.toData (), offerRedeemer, cxtToData]

cancelOfferSuccess :: Property
cancelOfferSuccess = property $ do
  seller <- forAll genPkh
  orderTxRef <- forAll genTxOutRef
  let 
    (token, _) = genAssets
    offerDatum = genDatum token seller 10
    offerRedeemer = mkOfferData CancelOffer
    txInfo = mkTxInfo seller
    purpose = mkPurpose orderTxRef
    cxtToData = PlutusTx.toData $ mkContext txInfo purpose
  expectSuccess [offerDatum, offerRedeemer, cxtToData]

cancelOfferWithWrongSigner :: Property
cancelOfferWithWrongSigner = property $ do
  seller <- forAll genPkh
  signer <- forAll genPkh
  orderTxRef <- forAll genTxOutRef
  let 
    (token, _) = genAssets
    offerDatum = genDatum token seller 10
    offerRedeemer = mkOfferData CancelOffer
    txInfo = mkTxInfo signer
    purpose = mkPurpose orderTxRef
    cxtToData = PlutusTx.toData $ mkContext txInfo purpose
  expectFailure [offerDatum, offerRedeemer, cxtToData]
