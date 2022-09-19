{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-imports for vs code compiler-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week01.Auth where
    
import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Int
import           Data.List            
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import qualified PlutusTx
import qualified Prelude              as Haskell
import           Ledger.Scripts       (Datum(..), DatumHash,)      
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints,typedValidatorLookups,mustPayToTheScript,adjustUnbalancedTx,mustIncludeDatum,mustPayToPubKey)
import qualified Ledger.Constraints   as Constraints 
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import qualified Prelude              as P
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), String)
import qualified Data.Text            as T
import           Text.Printf          (printf)
import qualified PlutusTx.Builtins   as Builtins


{-1 ada = 1 million lovelace. 1 ada ~ 25 rub
  min amount of ada in contract. spending all time, so after the first transfer to script
  user pays only fees-}
minLovelace :: Integer
minLovelace = 2000000


data AuData = AuData 
    { signatories :: [PaymentPubKeyHash]
    , numSignatures :: Integer
    } deriving stock (P.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

instance Eq AuData where
    {-# INLINABLE (==) #-}
    b == c = (signatories b == signatories c) 

PlutusTx.unstableMakeIsData ''AuData   
PlutusTx.makeLift ''AuData 

{-# INLINABLE mkValidator #-}
mkValidator :: AuData -> PaymentPubKeyHash -> ScriptContext -> Bool  {-comparison with old datum as i suppose-}
mkValidator gd pkh ctx = traceIfFalse "Min amount of L should be sent with each tx" (correctInputValue) &&
    traceIfFalse "Not sender or already first element in array"  (signedBySender pkh && notFirstEl pkh (signatories gd))  &&
    traceIfFalse "User registered before" (notInArray pkh (signatories gd)) &&
    traceIfFalse "Uncorrect output datum" (correctOutputDatum pkh gd)

  where
    notFirstEl :: (Eq PaymentPubKeyHash) => PaymentPubKeyHash -> [PaymentPubKeyHash] -> Bool
    notFirstEl _ [] = False
    notFirstEl x (y:ys) 
      | x/=y = True
      | otherwise = False
    
    notInArray :: (Eq PaymentPubKeyHash) => PaymentPubKeyHash -> [PaymentPubKeyHash] -> Bool
    notInArray _ [] = True
    notInArray a [b] 
        | a == b = False
        | otherwise = True
    notInArray a (x:xs)
      | a == x = False
      | otherwise = notInArray a xs

    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedBySender :: PaymentPubKeyHash ->  Bool
    signedBySender pk = txSignedBy info $ unPaymentPubKeyHash pk
    
    outputDatum :: AuData
    outputDatum = case getContinuingOutputs ctx of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "wrong output type"
            Just h -> case findDatum h info of
                Nothing        -> traceError "datum not found"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just da -> da
                    Nothing  -> traceError "error decoding data"
        _   -> traceError "expected exactly one continuing output"

    correctOutputDatum :: PaymentPubKeyHash -> AuData -> Bool
    correctOutputDatum pkh a = (numSignatures outputDatum == (numSignatures a) + 1) &&
                      (isFirstEl pkh $ signatories outputDatum)

    isFirstEl :: Eq PaymentPubKeyHash => PaymentPubKeyHash -> [PaymentPubKeyHash] -> Bool
    isFirstEl pkh (x:xs)
      | pkh == x = True
      | otherwise = False
    
    input :: TxInInfo
    input =
      let
        isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
            Nothing -> False
            Just _  -> True
        xs = [i | i <- txInfoInputs info, isScriptInput i]
      in
        case xs of
            [i] -> i
            _   -> traceError "expected exactly one script input"

    inVal :: Value
    inVal = txOutValue . txInInfoResolved $ input

    correctInputValue :: Bool
    correctInputValue = inVal == Ada.lovelaceValueOf minLovelace

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = AuData
    type instance RedeemerType Typed = PaymentPubKeyHash

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AuData @PaymentPubKeyHash

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data GiveData = GiveData
    { signs :: ![PaymentPubKeyHash] } deriving (Generic, ToJSON, FromJSON, ToSchema)

type AuthSchema =
            Endpoint "register" ()
        .\/ Endpoint "getAuthData" () 


append :: PaymentPubKeyHash -> [PaymentPubKeyHash] -> [PaymentPubKeyHash]
append a xs = [a] Data.List.++ xs 

findData :: Contract w s Text (TxOutRef, ChainIndexTxOut, AuData)
findData = do
    utxos <- utxosAt $ scrAddress
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             ]
    case xs of
        [] -> throwError "No previous datum"
        [(oref, o)] -> case _ciTxOutDatum o of
            Left _          -> throwError "datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "datum has wrong type"
                Just d@AuData{} -> return (oref, o, d)
       {- (((TxOutRef _ _), (PublicKeyChainIndexTxOut _ _)):_:_) -> throwError"No needed information"-}
     {-   (((TxOutRef _ _), (ScriptChainIndexTxOut _ _ _ _)):_:_) -> throwError "No needed information"-}
  
register :: Contract w s Text ()
register = do
    utxos <- utxosAt scrAddress
    pkh <- ownPaymentPubKeyHash 
    if Map.null utxos
        then do
            let lookups = typedValidatorLookups typedValidator
                tx = mustPayToTheScript dat $ Ada.lovelaceValueOf 1
                dat = AuData{signatories = [pkh],numSignatures=1}
            utx  <- mkTxConstraints lookups tx {- adjust is for flexibility of contract: if we wouldnt provide minLovelace but amount so the tx would add value of 2000000 - amount to be confirmed. like in my case. -}
            txid <- getCardanoTxId <$> submitUnbalancedTx (Constraints.adjustUnbalancedTx utx) 
            void $ awaitTxConfirmed txid
            logInfo @String $ printf "Contract initialized. Sender = %s" (P.show pkh)
        else do
            (oref, o, d@AuData{..}) <- findData  {-d = datum-}
            logInfo @P.String $ printf "Register 1: found utxo with datum %s" (P.show d)
            let newArr = append pkh signatories
                newData = d {signatories = newArr, numSignatures = numSignatures + 1}
                r  = Redeemer $ PlutusTx.toBuiltinData pkh
                lookups = typedValidatorLookups typedValidator <>
                    Constraints.unspentOutputs utxos <> {-or Map.singleton-}
                    Constraints.otherScript validator 
                tx = (mustPayToTheScript newData $ Ada.lovelaceValueOf minLovelace) <> 
                    Constraints.mustSpendScriptOutput oref r

            ledgerTx <- submitTxConstraintsWith lookups tx
            let txID = getCardanoTxId ledgerTx
            void $ awaitTxConfirmed txID
            logInfo @P.String $ printf "Register 2: got new datum %s" (P.show newData)
    
getAuthData :: Contract () AuthSchema T.Text ()
getAuthData = do          
    utxos <- utxosAt scrAddress
    {-let txOut = snd <$> Map.toList utxos  {- retrive last element of the tuple -} -}
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             ]
    case xs of
        [] -> logInfo @P.String $ printf "No previous datum"
        [(_, o)] -> case _ciTxOutDatum o of
            Left _          -> throwError "datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "datum has wrong type"
                Just d@AuData{} ->  logInfo @P.String $ printf "getAuthData: %s: " (P.show d) {-compiler's advice-}
        (((TxOutRef _ _), (PublicKeyChainIndexTxOut _ _)):_:_) -> logInfo @P.String $ printf "No needed information"
        (((TxOutRef _ _), (ScriptChainIndexTxOut _ _ _ _)):_:_) -> logInfo @P.String $ printf "No needed information"
                


endpoints :: Contract () AuthSchema Text ()
endpoints = awaitPromise (register' `select` getAuthData') >> endpoints
  where
    register' = endpoint @"register" $ const register
    getAuthData' = endpoint @"getAuthData" $ const getAuthData

mkSchemaDefinitions ''AuthSchema

mkKnownCurrencies []
