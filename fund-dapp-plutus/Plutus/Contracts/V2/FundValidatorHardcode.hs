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
    { beneficiary1 :: !PaymentPubKeyHash
    , beneficiary2 :: !PaymentPubKeyHash
    }


{-# INLINABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ ctx = traceIfFalse "You are not a beneficiary" signedByBeneficiary
                       && traceIfFalse "split is not done 50 50" isSplitInHalf
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        beneficiaries :: Benificiaries
        beneficiaries = Benificiaries {
            beneficiary1 = PaymentPubKeyHash "c77150227ca0cb67d597f415b0a424aa02246b54bb3bb03ec1667aa2",
            beneficiary2 = PaymentPubKeyHash "7e36a4c8da9c6ffaa842d69432c3008b76dd7a338111c55abe958dcc"
        }
    
        signedByBeneficiary1 :: Bool 
        signedByBeneficiary1 = txSignedBy info $ unPaymentPubKeyHash $ beneficiary1 beneficiaries

        signedByBeneficiary2 :: Bool
        signedByBeneficiary2 = txSignedBy info $ unPaymentPubKeyHash $ beneficiary2 beneficiaries

        signedByBeneficiary :: Bool
        signedByBeneficiary = signedByBeneficiary1 || signedByBeneficiary2

        checkIsSameAddress :: Address -> PaymentPubKeyHash -> Bool
        checkIsSameAddress addr paymentPubKeyHash = do
            let beneficiaryPubKeyHash = toPubKeyHash addr
            case beneficiaryPubKeyHash of
                Just a -> a == unPaymentPubKeyHash paymentPubKeyHash
                _ -> False

        isSplitInHalf :: Bool
        isSplitInHalf =
            let outputs =  txInfoOutputs info
                inputs  =  txInfoInputs  info
                split1 = foldr (\x acc ->
                            if checkIsSameAddress (txOutAddress x) (beneficiary1 beneficiaries) then acc + Value.valueOf (txOutValue x) adaSymbol adaToken else acc
                         ) 0 outputs
                split2 = foldr (\x acc ->
                            if checkIsSameAddress (txOutAddress x) (beneficiary1 beneficiaries) then acc + Value.valueOf (txOutValue x) adaSymbol adaToken else acc
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
