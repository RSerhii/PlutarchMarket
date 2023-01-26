module TestUtils (mkOfferRedeemer, mkOfferData, genToken1, genToken2, genPkh, genDatum, genAssets, mkTxInfo, mkPurpose, mkContext, genTxOutRef) where
    
import qualified PlutusTx

import Market.Types
import PlutusLedgerApi.V1
import qualified Data.Text as T
import PlutusLedgerApi.V1.Value
import PlutusTx.Builtins.Internal
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16  as Hex
import qualified Data.Text.Encoding      as E
import Hedgehog
import Hedgehog.Range as Range
import Hedgehog.Gen
import PlutusPrelude ((<&>))
import qualified PlutusLedgerApi.V1.Interval as Interval

mkOfferRedeemer :: OfferAction -> OfferRedeemer
mkOfferRedeemer = OfferRedeemer

mkOfferData :: OfferAction -> PlutusTx.Data
mkOfferData a = toData $ mkOfferRedeemer a

genToken1 :: TokenName
genToken1 = TokenName $ BuiltinByteString $ mkByteString $ T.pack "415f546f6b656e5f6e65775f706f6f6c0a"

genToken2 :: TokenName
genToken2 = TokenName $ BuiltinByteString $ mkByteString $ T.pack "425f546f6b656e5f6e65775f706f6f6c0a"

genCS :: CurrencySymbol
genCS = CurrencySymbol $ BuiltinByteString $ mkByteString $ T.pack "805fe1efcdea11f1e959eff4f422f118aa76dca2d0d797d184e487da"

mkAssetClass :: CurrencySymbol -> TokenName -> AssetClass
mkAssetClass cs tn = AssetClass (cs, tn)

genAssets :: (AssetClass, AssetClass)
genAssets = 
  let
    cs  = genCS
    t1   = genToken1
    t2   = genToken2
  in (mkAssetClass cs t1, mkAssetClass cs t2)

mkByteString :: T.Text -> BS.ByteString
mkByteString input = unsafeFromEither (Hex.decodeBase16 . E.encodeUtf8 $ input)

unsafeFromEither :: (Show b) => Either b a -> a
unsafeFromEither (Left err)    = Prelude.error ("Err:" ++ show err)
unsafeFromEither (Right value) = value

genPkh :: MonadGen f => f PubKeyHash
genPkh = genBuiltinByteString 28 <&> PubKeyHash

genBuiltinByteString :: MonadGen f => Int -> f BuiltinByteString
genBuiltinByteString s = bytes (Range.singleton s) <&> BuiltinByteString

genDatum :: AssetClass -> PubKeyHash -> Integer -> Data
genDatum token pkh price =
  let 
    config = mkOfferDatum token pkh price
  in toData config
  
mkOfferDatum :: AssetClass -> PubKeyHash -> Integer -> OfferDatum
mkOfferDatum = OfferDatum


mkTxInfo :: PubKeyHash -> TxInfo
mkTxInfo pkh =
  TxInfo
    { txInfoInputs = mempty
    , txInfoOutputs = mempty
    , txInfoFee = mempty
    , txInfoMint = mempty
    , txInfoDCert = []
    , txInfoWdrl = mempty
    , txInfoValidRange = Interval.always
    , txInfoSignatories = [pkh]
    , txInfoData = mempty
    , txInfoId = "b0"
    }

mkPurpose :: TxOutRef -> ScriptPurpose
mkPurpose = Spending

mkContext :: TxInfo -> ScriptPurpose -> ScriptContext
mkContext = ScriptContext

genTxOutRef :: MonadGen f => f TxOutRef
genTxOutRef = do
  txId <- genTxId
  ix   <- integral $ Range.constant 0 10
  pure $ TxOutRef txId ix

genTxId :: MonadGen f => f TxId
genTxId = prune $ random32bs <&> TxId

random32bs :: MonadGen f => f BuiltinByteString
random32bs = genBuiltinByteString 32
