{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Contract
where

import PlutusTx.Prelude
import PlutusTx hiding( txOutDatum)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V1.Ledger.Value as Value
import Ledger.Address


data Benificiaries = Benificiaries
    { beneficiary1 :: !Address
    , beneficiary2 :: !Address
    }

makeIsDataIndexed 'Benificiaries [('Benificiaries, 0)]

{-# INLINABLE mkValidator #-}
mkValidator :: Benificiaries -> () -> ScriptContext -> Bool
mkValidator (Benificiaries beneficiary1 beneficiary2) _ ctx = traceIfFalse "You are not a beneficiary" signedByBeneficiary
                       && traceIfFalse "split is not done 50 50" isSplitInHalf
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx
    
        signedByBeneficiary1 :: Bool 
        signedByBeneficiary1 = do
            let beneficiaryPubKeyHash = toPubKeyHash beneficiary1
            case beneficiaryPubKeyHash of
                Just a -> txSignedBy info a
                _ -> False

        signedByBeneficiary2 :: Bool
        signedByBeneficiary2 = do
            let beneficiaryPubKeyHash = toPubKeyHash beneficiary2
            case beneficiaryPubKeyHash of
                Just a -> txSignedBy info a
                _ -> False

        signedByBeneficiary :: Bool
        signedByBeneficiary = signedByBeneficiary1 || signedByBeneficiary2

        isSplitInHalf :: Bool
        isSplitInHalf =
            let outputs =  txInfoOutputs info
                inputs  =  txInfoInputs  info
                split1 = foldr (\x acc ->
                            if txOutAddress x == beneficiary1 then acc + Value.valueOf (txOutValue x) adaSymbol adaToken else acc
                         ) 0 outputs
                split2 = foldr (\x acc ->
                            if txOutAddress x == beneficiary2 then acc + Value.valueOf (txOutValue x) adaSymbol adaToken else acc
                         ) 0 outputs
                scriptValue = foldr (\x acc ->
                            acc + Value.valueOf (txOutValue $ txInInfoResolved x) adaSymbol adaToken
                         ) 0 inputs 
            in (split1 == split2) && (split1 + split2 == scriptValue)



{-# INLINABLE mkWrappedValidator #-}
mkWrappedValidator ::  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator  d r c = check $ mkValidator  (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)

validator :: Validator
validator  = mkValidatorScript  $$(PlutusTx.compile [|| mkWrappedValidator ||])
