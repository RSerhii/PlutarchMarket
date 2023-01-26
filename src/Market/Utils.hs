module Market.Utils
  (containsSignature, wrapValidator) where

import Plutarch.Prelude
import Plutarch.Api.V1 (PPubKeyHash)
import Plutarch.Api.V1.Contexts (PScriptContext)
import Plutarch.Unsafe (punsafeCoerce)


containsSignature :: Term s (PBuiltinList (PAsData PPubKeyHash) :--> PPubKeyHash :--> PBool)
containsSignature = phoistAcyclic $ plam $ \signatories userPubKeyHash -> pelem # pdata userPubKeyHash # signatories

wrapValidator ::
    (PIsData dt, PIsData rdmr) =>
    Term s (dt :--> rdmr :--> PScriptContext :--> PBool) ->
    Term s (PData :--> PData :--> PScriptContext :--> POpaque)
wrapValidator validator = plam $ \datum redeemer ctx ->
    let dt = pfromData $ punsafeCoerce datum
        rdmr = pfromData $ punsafeCoerce redeemer
        result = validator # dt # rdmr # ctx
     in popaque $ pif result (pcon PUnit) (ptraceError "Validator reduced to False")
