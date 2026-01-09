{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Stablecoin where

-- EXACT SAME IMPORTS as your original
import Prelude (IO, String, FilePath, putStrLn, (<>), take)
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken, AssetClass(..), assetClassValueOf)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum & Redeemer (Stablecoin Version)
------------------------------------------------------------------------

data VaultDatum = VaultDatum
    { vdOwner     :: PubKeyHash
    , vdCollateral :: Integer      -- ADA locked
    , vdDebt      :: Integer      -- Stablecoin minted (e.g., 1:0.67 ratio)
    , vdAsset     :: (CurrencySymbol, TokenName)  -- Stablecoin to mint
    }
PlutusTx.unstableMakeIsData ''VaultDatum

data VaultAction
    = MintStablecoin
    | RepayStablecoin
PlutusTx.unstableMakeIsData ''VaultAction

------------------------------------------------------------------------
-- Helpers (SAME as yours)
------------------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx =
    txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE getOwnInputValue #-}
getOwnInputValue :: ScriptContext -> Value
getOwnInputValue ctx =
    case findOwnInput ctx of
        Nothing -> traceError "no script input"
        Just i  -> txOutValue $ txInInfoResolved i

------------------------------------------------------------------------
-- Stablecoin Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkStablecoinValidator #-}
mkStablecoinValidator :: VaultDatum -> VaultAction -> ScriptContext -> Bool
mkStablecoinValidator dat action ctx =
    case action of
        -- Mint stablecoin against collateral
        MintStablecoin ->
            traceIfFalse "owner not signed" ownerSigned &&
            traceIfFalse "overcollateralization failed" validCollateralRatio &&
            traceIfFalse "stablecoins not minted to owner" stablecoinsMinted

        -- Repay stablecoin to unlock collateral
        RepayStablecoin ->
            traceIfFalse "owner not signed" ownerSigned &&
            traceIfFalse "stablecoins not burned" stablecoinsBurned
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownerSigned :: Bool
    ownerSigned = signedBy (vdOwner dat) ctx

    -- 150% collateral ratio: collateral >= debt * 1.5
    validCollateralRatio :: Bool
    validCollateralRatio =
        vdCollateral dat * 100 >= vdDebt dat * 150

    -- Verify stablecoins are minted to vault owner
    stablecoinsMinted :: Bool
    stablecoinsMinted =
        let (cs, tn) = vdAsset dat
            mintedValue = txInfoMint info
        in assetClassValueOf mintedValue (AssetClass (cs, tn)) == vdDebt dat &&
           valuePaidTo info (vdOwner dat) `geq` (Ada.lovelaceValueOf $ vdDebt dat)

    -- Verify stablecoins are burned (negative mint)
    stablecoinsBurned :: Bool
    stablecoinsBurned =
        let (cs, tn) = vdAsset dat
            mintedValue = txInfoMint info
        in assetClassValueOf mintedValue (AssetClass (cs, tn)) == negate (vdDebt dat)

------------------------------------------------------------------------
-- Untyped Wrapper (IDENTICAL pattern to yours)
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkStablecoinValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator =
    mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Serialization (SAME functions, just renamed)
------------------------------------------------------------------------

stablecoinValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
stablecoinValidatorHash val =
    let bytes  = Serialise.serialise val
        short  = SBS.toShort (LBS.toStrict bytes)
    in PlutusV2.ValidatorHash (toBuiltin (SBS.fromShort short))

stablecoinScriptAddress :: Address
stablecoinScriptAddress =
    Address
        (ScriptCredential (stablecoinValidatorHash validator))
        Nothing

toBech32StablecoinAddress :: C.NetworkId -> Validator -> String
toBech32StablecoinAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash   = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

stablecoinValidatorToCBORHex :: Validator -> String
stablecoinValidatorToCBORHex val =
    let bytes = LBS.toStrict $ Serialise.serialise val
    in BS.foldr (\b acc -> byteToHex b <> acc) "" bytes
  where
    hexChars = "0123456789abcdef"
    byteToHex b =
        let hi = P.fromIntegral b `P.div` 16
            lo = P.fromIntegral b `P.mod` 16
        in [ hexChars P.!! hi, hexChars P.!! lo ]

------------------------------------------------------------------------
-- File Writers (SAME as yours)
------------------------------------------------------------------------

writeStablecoinValidator :: FilePath -> Validator -> IO ()
writeStablecoinValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Stablecoin validator written to: " <> path

writeStablecoinCBOR :: FilePath -> Validator -> IO ()
writeStablecoinCBOR path val = do
    let bytes = LBS.toStrict (Serialise.serialise val)
        hex   = B16.encode bytes
    BS.writeFile path hex
    putStrLn $ "Stablecoin CBOR hex written to: " <> path

------------------------------------------------------------------------
-- Main (Same structure, different output names)
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeStablecoinValidator "stablecoin_mint.plutus" validator
    writeStablecoinCBOR      "stablecoin.cbor"        validator

    let vh      = stablecoinValidatorHash validator
        addr    = stablecoinScriptAddress
        bech32  = toBech32StablecoinAddress network validator
        cborHex = stablecoinValidatorToCBORHex validator

    putStrLn "\n--- Stablecoin Minting Protocol ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Script Address: " <> P.show addr
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn $ "CBOR Hex (first 120 chars): " <> P.take 120 cborHex <> "..."
    putStrLn "-----------------------------------"