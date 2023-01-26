{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Market.PTypes (
    POfferDatum (..),
    POfferRedeemer (..),
    POfferAction (..),
) where

import qualified GHC.Generics as GHC
-- import Generics.SOP (Generic, I (I))

import Plutarch
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.Prelude
import Plutarch.Api.V1 (PPubKeyHash)
import Plutarch.Internal.PlutusType (pcon',pmatch')
import Plutarch.Builtin (PIsData (..), pasInt, pforgetData)
import Plutarch.Unsafe (punsafeCoerce)
import PExtra.API
import Market.Types

-- PDatum

newtype POfferDatum (s :: S)
    = POfferDatum
        ( Term
            s
            ( PDataRecord
                '[ "token" ':= PAssetClass
                 , "seller" ':= PPubKeyHash
                 , "price" ':= PInteger
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving anyclass
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType POfferDatum where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl POfferDatum where type PLifted POfferDatum = OfferDatum
deriving via (DerivePConstantViaData OfferDatum POfferDatum) instance (PConstantDecl OfferDatum)

-- PRedeemer

data POfferAction (s :: S) = PAcceptOffer | PCancelOffer

instance PIsData POfferAction where
    pfromDataImpl tx =
        let x = pasInt # pforgetData tx
         in pmatch' x pcon
    pdataImpl x = pmatch x (punsafeCoerce . pdata . pcon')

instance PlutusType POfferAction where
    type PInner POfferAction = PInteger

    pcon' PAcceptOffer  = 0
    pcon' PCancelOffer = 1

    pmatch' x f =
        pif (x #== 0) (f PAcceptOffer) (f PCancelOffer)

newtype POfferRedeemer (s :: S)
    = POfferRedeemer
        ( Term
            s
            ( PDataRecord
                '[ "action" ':= POfferAction
                 ]
            )
        )
    deriving stock (GHC.Generic)
    deriving anyclass
        (PIsData, PDataFields, PlutusType)

instance DerivePlutusType POfferRedeemer where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl POfferRedeemer where type PLifted POfferRedeemer = OfferRedeemer
deriving via (DerivePConstantViaData OfferRedeemer POfferRedeemer) instance (PConstantDecl OfferRedeemer)
