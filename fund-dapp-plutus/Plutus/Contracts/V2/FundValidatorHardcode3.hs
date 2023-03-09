{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}



module Contract
where

import PlutusTx.Prelude
import PlutusTx hiding( txOutDatum)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V1.Ledger.Value as Value
import Plutus.V1.Ledger.Address
import Ledger.Address (unPaymentPubKeyHash, PaymentPubKeyHash (..))
import Ledger.Typed.Scripts as Scripts

data Beneficiaries = Beneficiaries
    { beneficiary1 :: PaymentPubKeyHash
    , beneficiary2 :: PaymentPubKeyHash
    }

PlutusTx.makeLift ''Beneficiaries 

{-# INLINABLE mkValidator #-}
mkValidator :: Beneficiaries -> () -> () -> ScriptContext -> Bool
mkValidator p _ _ ctx = traceIfFalse "split is not done 50 50" isSplitInHalf
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        checkIsValidatorUtxo :: TxInInfo -> ValidatorHash -> Bool
        checkIsValidatorUtxo tx valHash = do
            case toValidatorHash $ txOutAddress $ txInInfoResolved tx of
                Just a -> a == valHash
                _ -> False

        checkIsSameAddress :: Address -> PubKeyHash -> Bool
        checkIsSameAddress addr pubKeyHash = do
            case toPubKeyHash addr of
                Just a -> a == pubKeyHash
                _ -> False

        isSplitInHalf :: Bool
        isSplitInHalf =
            let outputs =  txInfoOutputs info
                inputs  =  txInfoInputs  info
                --inputs' = foldr (\x acc -> if (checkIsValidatorUtxo x $ ownHash ctx) then x:acc else acc) [] inputs
                
                split1 = foldr (\x acc ->
                            if checkIsSameAddress (txOutAddress x) (unPaymentPubKeyHash $ beneficiary1 p) 
                                then acc + Value.valueOf (txOutValue x) adaSymbol adaToken 
                            else acc
                         ) 0 outputs

                split2 = foldr (\x acc ->
                            if checkIsSameAddress (txOutAddress x) (unPaymentPubKeyHash $ beneficiary2 p) 
                                then acc + Value.valueOf (txOutValue x) adaSymbol adaToken 
                            else acc
                         ) 0 outputs

                scriptValue = foldr (\x acc ->
                            if checkIsValidatorUtxo x $ ownHash ctx then acc + Value.valueOf (txOutValue $ txInInfoResolved x) adaSymbol adaToken else acc
                         ) 0 inputs

            in (split1 == split2) && (split1 + split2 == scriptValue)


{-# INLINABLE wrapValidator #-}
wrapValidator :: Beneficiaries -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator cp _ _ ctx = check $ mkValidator cp  () () (parseData ctx "wrong input")
    where parseData a s = case fromBuiltinData a of
            Just x  -> x
            Nothing -> traceError s

validator' :: Beneficiaries -> Validator
validator' p =  mkValidatorScript $ $$(compile [|| wrapValidator ||]) `applyCode` liftCode p

validator :: Validator
validator = validator' $ Beneficiaries (PaymentPubKeyHash "c77150227ca0cb67d597f415b0a424aa02246b54bb3bb03ec1667aa2") (PaymentPubKeyHash "7e36a4c8da9c6ffaa842d69432c3008b76dd7a338111c55abe958dcc")
