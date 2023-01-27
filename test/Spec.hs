module Spec (spec) where

import Test.Tasty
import Test.Tasty.Hedgehog as HH
import Hedgehog
import qualified PlutusTx
import Eval (expectFailure, expectSuccess)
import Market.Types
import TestUtils
import PlutusLedgerApi.V1 (toData)

spec :: TestTree
spec =
  testGroup
    "marketValidator tests"
    [
      cancelOfferTestGroup,
      acceptOfferTestGroup
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
  offerTxRef <- forAll genTxOutRef
  let 
    offerRedeemer = mkOfferData CancelOffer
    txInfo = mkTxInfo seller
    purpose = mkPurpose offerTxRef
    cxtToData = PlutusTx.toData $ mkContext txInfo purpose
  expectFailure [PlutusTx.toData (), offerRedeemer, cxtToData]

cancelOfferSuccess :: Property
cancelOfferSuccess = property $ do
  seller <- forAll genPkh
  offerTxRef <- forAll genTxOutRef
  let 
    (token, _) = genAssets
    offerDatum = genDatum token seller 10
    offerRedeemer = mkOfferData CancelOffer
    txInfo = mkTxInfo seller
    purpose = mkPurpose offerTxRef
    cxtToData = PlutusTx.toData $ mkContext txInfo purpose
  expectSuccess [offerDatum, offerRedeemer, cxtToData]

cancelOfferWithWrongSigner :: Property
cancelOfferWithWrongSigner = property $ do
  seller <- forAll genPkh
  signer <- forAll genPkh
  offerTxRef <- forAll genTxOutRef
  let 
    (token, _) = genAssets
    offerDatum = genDatum token seller 10
    offerRedeemer = mkOfferData CancelOffer
    txInfo = mkTxInfo signer
    purpose = mkPurpose offerTxRef
    cxtToData = PlutusTx.toData $ mkContext txInfo purpose
  expectFailure [offerDatum, offerRedeemer, cxtToData]


acceptOfferTestGroup :: TestTree
acceptOfferTestGroup = testGroup "acceptOffer"
  [ 
    HH.testProperty "acceptOfferSuccess" acceptOfferSuccess,
    HH.testProperty "acceptOfferNotEnoughTokens" acceptOfferNotEnoughTokens,
    HH.testProperty "acceptOfferNoTokensForSeller" acceptOfferNoTokensForSeller,
    HH.testProperty "acceptOfferSomeTokensForSeller" acceptOfferSomeTokensForSeller
  ]
  
acceptOfferSuccess :: Property
acceptOfferSuccess = property $ do
  seller <- forAll genPkh
  buyer <- forAll genPkh
  script <- forAll genPkh
  offerTxRef <- forAll genTxOutRef
  buyerTxRef <- forAll genTxOutRef
  let 
    (token, token2) = genAssets
    offerDatum = mkOfferDatum token seller 10
    offerRedeemer = mkOfferData AcceptOffer
    
    datumHash = mkDatumHash $ mkDatum offerDatum

    offerTxIn = genTxIn offerTxRef datumHash token2 15 script
    buyerTxIn = genTxIn buyerTxRef datumHash token 10 buyer
    
    sellerTxOut = genTxOut datumHash token 10 seller
    buyerTxOut = genTxOut datumHash token2 15 buyer
    
    txInfo = mkTxInfoWithIO [offerTxIn, buyerTxIn] [sellerTxOut, buyerTxOut] seller
    purpose = mkPurpose offerTxRef
    cxtToData = PlutusTx.toData $ mkContext txInfo purpose
  expectSuccess [toData offerDatum, offerRedeemer, cxtToData]

acceptOfferNotEnoughTokens :: Property
acceptOfferNotEnoughTokens = property $ do
  seller <- forAll genPkh
  buyer <- forAll genPkh
  script <- forAll genPkh
  offerTxRef <- forAll genTxOutRef
  buyerTxRef <- forAll genTxOutRef
  let 
    (token, token2) = genAssets
    offerDatum = mkOfferDatum token seller 10
    offerRedeemer = mkOfferData AcceptOffer
    
    datumHash = mkDatumHash $ mkDatum offerDatum

    offerTxIn = genTxIn offerTxRef datumHash token2 15 script
    buyerTxIn = genTxIn buyerTxRef datumHash token 9 buyer
    
    sellerTxOut = genTxOut datumHash token 9 seller
    buyerTxOut = genTxOut datumHash token2 15 buyer
    
    txInfo = mkTxInfoWithIO [offerTxIn, buyerTxIn] [sellerTxOut, buyerTxOut] seller
    purpose = mkPurpose offerTxRef
    cxtToData = PlutusTx.toData $ mkContext txInfo purpose
  expectFailure [toData offerDatum, offerRedeemer, cxtToData]
  
acceptOfferNoTokensForSeller :: Property
acceptOfferNoTokensForSeller = property $ do
  seller <- forAll genPkh
  buyer <- forAll genPkh
  script <- forAll genPkh
  offerTxRef <- forAll genTxOutRef
  buyerTxRef <- forAll genTxOutRef
  let 
    (token, token2) = genAssets
    offerDatum = mkOfferDatum token seller 10
    offerRedeemer = mkOfferData AcceptOffer
    
    datumHash = mkDatumHash $ mkDatum offerDatum

    offerTxIn = genTxIn offerTxRef datumHash token2 15 script
    buyerTxIn = genTxIn buyerTxRef datumHash token 10 buyer
    
    sellerTxOut = genTxOut datumHash token 10 buyer
    buyerTxOut = genTxOut datumHash token2 15 buyer
    
    txInfo = mkTxInfoWithIO [offerTxIn, buyerTxIn] [sellerTxOut, buyerTxOut] seller
    purpose = mkPurpose offerTxRef
    cxtToData = PlutusTx.toData $ mkContext txInfo purpose
  expectFailure [toData offerDatum, offerRedeemer, cxtToData]

acceptOfferSomeTokensForSeller :: Property
acceptOfferSomeTokensForSeller = property $ do
  seller <- forAll genPkh
  buyer <- forAll genPkh
  script <- forAll genPkh
  offerTxRef <- forAll genTxOutRef
  buyerTxRef <- forAll genTxOutRef
  let 
    (token, token2) = genAssets
    offerDatum = mkOfferDatum token seller 10
    offerRedeemer = mkOfferData AcceptOffer
    
    datumHash = mkDatumHash $ mkDatum offerDatum

    offerTxIn = genTxIn offerTxRef datumHash token2 15 script
    buyerTxIn = genTxIn buyerTxRef datumHash token 10 buyer
    
    sellerTxOut = genTxOut datumHash token 9 seller
    buyerTxOut = genTxOut datumHash token 1 buyer
    buyerTxOut2 = genTxOut datumHash token2 15 buyer
    
    txInfo = mkTxInfoWithIO [offerTxIn, buyerTxIn] [sellerTxOut, buyerTxOut, buyerTxOut2] seller
    purpose = mkPurpose offerTxRef
    cxtToData = PlutusTx.toData $ mkContext txInfo purpose
  expectFailure [toData offerDatum, offerRedeemer, cxtToData]
