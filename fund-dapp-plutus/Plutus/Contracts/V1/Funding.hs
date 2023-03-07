{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Funding where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)

data Benificiary = Beneficiary
	{ pubKeyHash :: PaymentPubKeyHash
 	, amount     :: Integer
	} deriving Show

data Benificiaries = Benificiaries
    { beneficiary1 :: PaymentPubKeyHash
	, beneficiary2 :: PaymentPubKeyHash
	} deriving Show

{-# INLINABLE mkValidator #-}
mkValidator :: Benificiaries -> () -> ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "You are not a beneficiary" signedByBeneficiary  
					   && traceIfFalse "split is not done 50 50" isSplitInHalf
    where
		info :: TxInfo
		info = scriptContextTxInfo ctx
		
		signedByBeneficiary1 :: Bool
		signedByBeneficiary1 = txSignedBy info $ unPaymentPubKeyHash $ beneficiary1 dat

		signedByBeneficiary2 :: Bool
		signedByBeneficiary2 = txSignedBy info $ unPaymentPubKeyHash $ beneficiary2 dat

        signedByBeneficiary :: Bool
		signedByBeneficiary = signedByBeneficiary1 || signedByBeneficiary2

		checkBeneficiary1 :: Bool
        checkBeneficiary1 = txSignedBy info $ unPaymentPubKeyHash $ bpBeneficiary1 dat

        checkBeneficiary2 :: Bool
        checkBeneficiary2 = txSignedBy info $ unPaymentPubKeyHash $ bpBeneficiary2 dat

        isSplitInHalf :: Bool
        isSplitInHalf =
          let outputs =  txInfoOutptus $ info
              inputs = txInfoInputs $ info
              split1 = foldr (\x acc ->
                                if (txOutAddress x == bpBeneficiary1 dat) then acc + (valueOf (txOutValue x) adaSymbol adaToken )
                        ) 0 outputs
              split2 =  foldr (\x acc ->
                                if (txOutAddress x == bpBeneficiary2 dat) then acc + (valueOf (txOutValue x) adaSymbol adaToken)
                        ) 0 outputs
              scriptValue = foldr (\x acc ->
                                    acc + (valueOf (txOutValue (txInInfoResolved x)) adaSymbol adaToken)
                        ) 0 inputs
          in split1 == split2 == scriptValue/2


data Funding
instance Scripts.ValidatorTypes Funding where
	type instance DatumType Funding = Benificiaries
	type instance RedeemerType Funding = ()


typedValidator :: Scripts.TypedValidator Vesting
typedValidator p = Scripts.mkTypedValidator @Funding
    $$(PlutusTx.complie [|| mkValidator ||])
	$$(PlutusTx.complie [|| wrap ||])
  where
	wrap = Scripts.wrapValidator @Benificiaries @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.validatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data PayParams = PayParams
    { beneficiary1 :: !PaymentPubKeyHash
	, beneficiary2 :: !PaymentPubKeyHash
	, amount 	   :: !Integer
	} deriving (Generic, ToJSON, FromJSON, ToSchema)

type FundingSchema =
		Endpoint "pay" PayParams
	.\/ Endpoint "redeem" ()

pay :: AsContractError e => GiveParams -> Contract w s e ()
pay gp = do
	let dat = Benificiaries
			{ beneficiary1 =  beneficiary1 gp
			, beneficiary2 = beneficiary2 gp
   			}
		tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ amount gp
	ledgerTx <- submitTxConstraints typedValidator tx
	void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
	loginfo @String $ printf "made a payment of %d lovelace to script"
		(amount gp)

redeem :: forall w s e. AsContractError e => () -> Contract w s e ()
redeem = do
	pkh <- ownPaymentPubKeyHash
	utxos <- Map.filter (isSuitable pkh) <$> utxosAt scrAddress
	if Map.null utxos
		then logInfo @String $ "No one has payed"
		else do
			let orefs   = fst <$> Map.toList utxos
			    lookups = Constraints.unspentOutputs utxos  <>
					      Constraints.otherScript validator
				datum <- getDatum
				tx'     = [[Constraints.mustPayToPubKey (beneficiary1 datum) Ada.lovelaceValueOf utxosmoney/2, Constraints.mustPayToPubKey (beneficiary2 datum) Ada.lovelaceValueOf utxosmoney/2,]] | oref <- orefs] ----------- what to do ?? utxosmoney ??
				tx :: TxConstraints Void Void
				tx      = mconcat (foldl (\acc (a:b:[]) -> a : b : acc) [] tx') 
			ledgerTx <- submitTxConstraintsWith @Void lookups tx
			void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
			logInfo @String $ "Redeemed funds"
	where
		isSuitable :: PaymentPubKeyHash -> ChainIndexTxOut -> Bool
		isSuitable pkh o = case _ciTxOutDatum o of
					   Left _ 			-> False
					   Right (Datum e)  -> case PlutusTx.fromBuiltinData e of
							  Nothing -> False
							  Just d  -> beneficiary1 == pkh || beneficiary2 == pkh

		getDatum :: ChainIndexTxOut -> Benificiaries 
		getDatum o = case _ciTxOutDatum o of
				 	Left _ 			-> Nothing
					Right (Datum e) -> case PlutusTx.fromBuiltinData e of
						  Nothing -> Nothing
						  Just d  -> Just d
	

endpoints :: Contract () FundingSchema Text ()
endpoints = awaitPromise (pay' `select` redeem) >> endpoints
	where
		pay'    = endpoint @"pay" pay
		redeem' = endpoint @"redeem" redeem
	
mkSchemaDefinitions ''FundingSchema

mkKnownCurrencies []

