{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Market.Types (
  OfferDatum (..),
  OfferRedeemer (..),
  OfferAction (..),
) where

import PlutusLedgerApi.V1.Crypto (PubKeyHash)
import PlutusLedgerApi.V1.Value
import qualified PlutusTx
import PlutusTx.Builtins
import PlutusTx.IsData.Class
    ( ToData(..), FromData(..), UnsafeFromData(..) )

-- Datum

data OfferDatum = OfferDatum
    { token :: AssetClass
    , seller :: PubKeyHash
    , price :: Integer
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''OfferDatum [('OfferDatum, 0)]

-- Redeemer


data OfferAction = AcceptOffer | CancelOffer
    deriving stock (Show)

instance FromData OfferAction where
    {-# INLINE fromBuiltinData #-}
    fromBuiltinData d = matchData' d (\_ _ -> Nothing) (const Nothing) (const Nothing) chooseAction (const Nothing)
      where
        chooseAction i
            | i == 0 = Just AcceptOffer
            | i == 1 = Just CancelOffer
            | otherwise = Nothing

instance UnsafeFromData OfferAction where
    {-# INLINE unsafeFromBuiltinData #-}
    unsafeFromBuiltinData = maybe (Prelude.error "") id . fromBuiltinData

instance ToData OfferAction where
    {-# INLINE toBuiltinData #-}
    toBuiltinData a = mkI $ case a of
        AcceptOffer -> 0
        CancelOffer -> 1

data OfferRedeemer = OfferRedeemer
    { action      :: OfferAction
    }
    deriving stock (Show)

PlutusTx.makeIsDataIndexed ''OfferRedeemer [('OfferRedeemer, 0)]
