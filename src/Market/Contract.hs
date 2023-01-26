{-# OPTIONS_GHC -Wno-unused-matches #-}
module Market.Contract (marketValidator) where

import Plutarch.Prelude
import Plutarch.Api.V1.Contexts
import Plutarch.Extra.TermCont

import Market.PTypes (POfferDatum, POfferRedeemer, POfferAction (..))
import Market.Utils
import PExtra.API

marketValidator :: ClosedTerm (POfferDatum :--> POfferRedeemer :--> PScriptContext :--> PBool)
marketValidator = plam $ \datum' redeemer' ctx' -> unTermCont $ do
    -- Parse data
    ctx  <- pletFieldsC @'["txInfo", "purpose"] ctx'
    datum <- pletFieldsC @'["token", "seller", "price"] datum'
    redeemer <- pletFieldsC @'["action"] redeemer'
    let 
        seller = getField @"seller" datum
        -- token = getField @"token" datum
        -- price = getField @"price" datum
        txInfo' = getField @"txInfo" ctx
        action = getField @"action" redeemer
    
    txInfo  <- pletFieldsC @'["inputs", "outputs", "signatories"] txInfo'
    inputs  <- tletUnwrap $ getField @"inputs"  txInfo
    outputs <- tletUnwrap $ getField @"outputs" txInfo

    -- Actions
    
    pure $
        pmatch action $ \case
            PAcceptOffer -> seller #== seller -- stub
            PCancelOffer ->
                let sigs = pfromData $ getField @"signatories" txInfo
                 in containsSignature # sigs # seller
