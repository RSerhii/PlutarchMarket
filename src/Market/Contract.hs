{-# OPTIONS_GHC -Wno-unused-matches #-}
module Market.Contract (marketValidator) where

import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import Plutarch.Extra.TermCont

import Market.PTypes (POfferDatum, POfferRedeemer, POfferAction (..))
import Market.Utils
import PExtra.API
import Plutarch.Api.V1
import PExtra.Monadic (tletField, tmatchField)

marketValidator :: ClosedTerm (POfferDatum :--> POfferRedeemer :--> PScriptContext :--> PBool)
marketValidator = plam $ \datum' redeemer' ctx' -> unTermCont $ do
    redeemer <- pletFieldsC @'["action"] redeemer'
    let action = getField @"action" redeemer
    pure $
        pmatch action $ \case
            PAcceptOffer -> isOfferCompleted # datum' # ctx'
            PCancelOffer -> isOfferCanceled # datum' # ctx'

isOfferCanceled :: Term s (POfferDatum :--> PScriptContext :--> PBool)
isOfferCanceled = plam $ \datum' ctx' -> unTermCont $ do
    ctx  <- pletFieldsC @'["txInfo"] ctx'
    datum <- pletFieldsC @'["seller"] datum'
    let 
        seller = getField @"seller" datum
        txInfo' = getField @"txInfo" ctx
    txInfo  <- pletFieldsC @'["signatories"] txInfo'
    let sigs = pfromData $ getField @"signatories" txInfo
        in pure $ containsSignature # sigs # seller

isOfferCompleted :: Term s (POfferDatum :--> PScriptContext :--> PBool)
isOfferCompleted = plam $ \datum' ctx' -> unTermCont $ do
    ctx  <- pletFieldsC @'["txInfo", "purpose"] ctx'
    datum <- pletFieldsC @'["token", "seller", "price"] datum'
    let 
        seller = getField @"seller" datum
        token = getField @"token" datum
        price = getField @"price" datum
        txInfo' = getField @"txInfo" ctx
    
    txInfo  <- pletFieldsC @'["inputs", "outputs"] txInfo'
    inputs  <- tletUnwrap $ getField @"inputs"  txInfo
    outputs <- tletUnwrap $ getField @"outputs" txInfo

    let
        hasTokens = txInHasTokens # token # price # inputs
        sellerReceivedTokens = txOutHasSellerOutput # token # price # seller # outputs

    pure $ hasTokens #&& sellerReceivedTokens

txInHasTokens :: Term s (PAssetClass :--> PInteger :--> PBuiltinList PTxInInfo :--> PBool)
txInHasTokens = phoistAcyclic $ plam $ \token price -> 
    precList
        ( \self x xs ->
            let actualValue = getAssetValueFromTx # token # x
                in pif (actualValue #== price) (pcon PTrue) (self # xs)
        )
        (const $ ptraceError "Inputs not found")
        
getAssetValueFromTx :: Term s (PAssetClass :--> PTxInInfo :--> PInteger)
getAssetValueFromTx = phoistAcyclic $ plam $ \token tx -> unTermCont $ do
    txIn <- pletFieldsC @'["resolved"] tx
    value <-
        let resolvedTx = pfromData $ getField @"resolved" txIn
        in tletField @"value" resolvedTx
    pure $ assetClassValueOf # value # token

txOutHasSellerOutput :: Term s (PAssetClass :--> PInteger :--> PPubKeyHash :--> PBuiltinList PTxOut :--> PBool)
txOutHasSellerOutput = phoistAcyclic $ plam $ \token price seller ->
    precList
        ( \self x xs ->
            let value = pfield @"value" # x
                actualValue   = assetClassValueOf # value # token
                in pif (outputPaysTo # seller # x) (actualValue #== price) (self # xs)
        )
        (const $ ptraceError "Seller's output not found")


outputPaysTo :: Term s (PPubKeyHash :--> PTxOut :--> PBool)
outputPaysTo = plam $ \pkh txout -> unTermCont $ do
    PTxOut txout' <- pmatchC txout
    PAddress adr <- tmatchField @"address" txout'
    pure $
        pmatch (pfromData $ pfield @"credential" # adr) $ \case
            PPubKeyCredential cred -> unTermCont $ do
                pkhOut <- tletField @"_0" cred
                pure $ pkhOut #== pkh
            _ -> pcon PFalse
