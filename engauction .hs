{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Week01.EnglishAuction
    ( Auction (..)
    , StartParams (..), BidParams (..), CloseParams (..)
    , AuctionSchema
    , start, bid, close
    , endpoints
    , schemas
    , ensureKnownCurrencies
    , printJson
    , printSchemas
    , registeredKnownCurrencies
    , stage
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P
import           Schema               (ToSchema)
import           Text.Printf          (printf)

minLovelace :: Integer
minLovelace = 2000000         {--контракт должен содержать какое-то минимальное количество ADA--}

data Auction = Auction             {-- информация об аукционе: у кого самая высокая ставка, тот получает нфт токен --}
    { aSeller   :: !PaymentPubKeyHash              {--кто продает нфт--}
    , aDeadline :: !POSIXTime     {--через сколько слотов происходит закрытие аукциона --}
    , aMinBid   :: !Integer    {--стартовая мин ставка--}
    , aCurrency :: !CurrencySymbol  {--символ нфт-токена, который достается победителю --}
    , aToken    :: !TokenName   
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq Auction where  {--определяет совпадение информации об аукционах--}
    {-# INLINABLE (==) #-}        {--как определение функции, нужно для того, чтобы код достиг компилятора Plutus(прежде ghci). Для перевода в Plutus Core--}
    a == b = (aSeller   a == aSeller   b) &&
             (aDeadline a == aDeadline b) &&
             (aMinBid   a == aMinBid   b) &&
             (aCurrency a == aCurrency b) &&
             (aToken    a == aToken    b)

PlutusTx.unstableMakeIsData ''Auction   {--instance аукциона?--}
PlutusTx.makeLift ''Auction {--to generate things like
    `instance Lift a => Lift (Maybe a)`--}

data Bid = Bid                   {-- участник и его ставка--}
    { bBidder :: !PaymentPubKeyHash
    , bBid    :: !Integer
    } deriving P.Show

instance Eq Bid where  {--эквивалента ли ставка(совпадает ли)--}
    {-# INLINABLE (==) #-}
    b == c = (bBidder b == bBidder c) &&
             (bBid    b == bBid    c)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

data AuctionAction = MkBid Bid | Close     {--тип аукциона, reedemer - валидирует input--}
    deriving P.Show

PlutusTx.unstableMakeIsData ''AuctionAction
PlutusTx.makeLift ''AuctionAction

data AuctionDatum = AuctionDatum  {--datum: used to carry script state information such as its owner or the timing details (which define when the UTXO can be spent)--}
    { adAuction    :: !Auction  {--информация об аукционе--}
    , adHighestBid :: !(Maybe Bid) {--у кого самая большая ставка + ее value --}
    } deriving P.Show

PlutusTx.unstableMakeIsData ''AuctionDatum
PlutusTx.makeLift ''AuctionDatum

data Auctioning
instance Scripts.ValidatorTypes Auctioning where {--определяет тип datum и reedemer--}
    type instance RedeemerType Auctioning = AuctionAction
    type instance DatumType Auctioning = AuctionDatum

{-# INLINABLE minBid #-}
minBid :: AuctionDatum -> Integer
minBid AuctionDatum{..} = case adHighestBid of
    Nothing      -> aMinBid adAuction
    Just Bid{..} -> bBid + 1

{-# INLINABLE mkAuctionValidator #-}
mkAuctionValidator :: AuctionDatum -> AuctionAction -> ScriptContext -> Bool {--подтверждение каждой созданной транзакции--}
mkAuctionValidator ad redeemer ctx =   {--auction datum, redeemer, (txInfo, ScriptPurpose)--}
    traceIfFalse "wrong input value" correctInputValue &&
    case redeemer of
        MkBid b@Bid{..} ->  {--из reedemer "AuctionAction" смотрим ставку (и поля)--}
            traceIfFalse "bid too low"        (sufficientBid bBid)         &&  {-- проверка на то, что ставка больше минимальной(=прошлой, как понимаю. тк аукцион)--}
            traceIfFalse "wrong output datum" (correctBidOutputDatum b)    &&
            traceIfFalse "wrong output value" (correctBidOutputValue bBid) && {--проверка на == (minLovelace + amount)--}
            traceIfFalse "wrong refund"       correctBidRefund             &&
            traceIfFalse "too late"           correctBidSlotRange
        Close           ->
            traceIfFalse "too early" correctCloseSlotRange &&
            case adHighestBid ad of
                Nothing      ->
                    traceIfFalse "expected seller to get token" (getsValue (aSeller auction) $ tokenValue <> Ada.lovelaceValueOf minLovelace)
                Just Bid{..} ->
                    traceIfFalse "expected highest bidder to get token" (getsValue bBidder $ tokenValue <> Ada.lovelaceValueOf minLovelace) &&
                    traceIfFalse "expected seller to get highest bid" (getsValue (aSeller auction) $ Ada.lovelaceValueOf bBid)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

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

    auction :: Auction
    auction = adAuction ad

    tokenValue :: Value
    tokenValue = Value.singleton (aCurrency auction) (aToken auction) 1

    correctInputValue :: Bool
    correctInputValue = inVal == case adHighestBid ad of
        Nothing      -> tokenValue <> Ada.lovelaceValueOf minLovelace
        Just Bid{..} -> tokenValue <> Ada.lovelaceValueOf (minLovelace + bBid)

    sufficientBid :: Integer -> Bool
    sufficientBid amount = amount >= minBid ad

    ownOutput   :: TxOut
    outputDatum :: AuctionDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "wrong output type"
            Just h -> case findDatum h info of
                Nothing        -> traceError "datum not found"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ad' -> (o, ad')
                    Nothing  -> traceError "error decoding data"
        _   -> traceError "expected exactly one continuing output"

    correctBidOutputDatum :: Bid -> Bool
    correctBidOutputDatum b = (adAuction outputDatum == auction)   &&
                              (adHighestBid outputDatum == Just b)

    correctBidOutputValue :: Integer -> Bool
    correctBidOutputValue amount =
        txOutValue ownOutput == tokenValue <> Ada.lovelaceValueOf (minLovelace + amount)

    correctBidRefund :: Bool
    correctBidRefund = case adHighestBid ad of
        Nothing      -> True
        Just Bid{..} ->
          let
            os = [ o
                 | o <- txInfoOutputs info
                 , txOutAddress o == pubKeyHashAddress bBidder Nothing
                 ]
          in
            case os of
                [o] -> txOutValue o == Ada.lovelaceValueOf bBid
                _   -> traceError "expected exactly one refund output"

    correctBidSlotRange :: Bool
    correctBidSlotRange = to (aDeadline auction) `contains` txInfoValidRange info

    correctCloseSlotRange :: Bool
    correctCloseSlotRange = from (aDeadline auction) `contains` txInfoValidRange info

    getsValue :: PaymentPubKeyHash -> Value -> Bool
    getsValue h v =
      let
        [o] = [ o'
              | o' <- txInfoOutputs info
              , txOutValue o' == v
              ]
      in
        txOutAddress o == pubKeyHashAddress h Nothing

typedAuctionValidator :: Scripts.TypedValidator Auctioning      {--для компилирования в Plutus Core--}
typedAuctionValidator = Scripts.mkTypedValidator @Auctioning
    $$(PlutusTx.compile [|| mkAuctionValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AuctionDatum @AuctionAction

auctionValidator :: Validator
auctionValidator = Scripts.validatorScript typedAuctionValidator

auctionHash :: Ledger.ValidatorHash
auctionHash = Scripts.validatorHash typedAuctionValidator

auctionAddress :: Ledger.Address
auctionAddress = scriptHashAddress auctionHash

data StartParams = StartParams             {--параметры(вход) для функции start, как endpoint--}
    { spDeadline :: !POSIXTime
    , spMinBid   :: !Integer
    , spCurrency :: !CurrencySymbol
    , spToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data BidParams = BidParams                 {--параметры(вход) для функции bid, как endpoint--}
    { bpCurrency :: !CurrencySymbol 
    , bpToken    :: !TokenName
    , bpBid      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
 
data CloseParams = CloseParams             {--параметры(вход) для функции close, как endpoint--}
    { cpCurrency :: !CurrencySymbol 
    , cpToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type AuctionSchema =                       {--интерфейс пользователя, перечисление endpoints--}
        Endpoint "start" StartParams
    .\/ Endpoint "bid"   BidParams
    .\/ Endpoint "close" CloseParams

start :: AsContractError e => StartParams -> Contract w s e () {--старт аукциона--}
start StartParams{..} = do
    pkh <- ownPaymentPubKeyHash
    let a = Auction                        {--добавления описания к контракту--}
                { aSeller   = pkh          {--PaymentPubKeyHash--}
                , aDeadline = spDeadline
                , aMinBid   = spMinBid
                , aCurrency = spCurrency   {--символ нфт-токена, который достается победителю --}
                , aToken    = spToken      {--имя токена--}
                }
        d = AuctionDatum
                { adAuction    = a
                , adHighestBid = Nothing   {--сначала самой высокой ставки нет, тк еще ни одна ставка не была сделана--}
                }
                                           {--Value.singleton: Make a 'Value' containing only the given quantity of the given currency--}
        v = Value.singleton spCurrency spToken 1 <> Ada.lovelaceValueOf minLovelace {--Все деньги на смарте = 1 нфт-токен + minLovelace. "<>" - грубо говоря склейка двух "value". lovelaceValueOf - перевод в тип Value--}
        tx = Constraints.mustPayToTheScript d v {--locks the value v with a script alongside a datum d--}
    ledgerTx <- submitTxConstraints typedAuctionValidator tx {-- examines the transaction constraints tx and builds a transaction that fulfills them <-компилирование в Plutus Core--}
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "started auction %s for token %s" (P.show a) (P.show v)

bid :: forall w s. BidParams -> Contract w s Text ()      {--создание ставки--}
bid BidParams{..} = do
    (oref, o, d@AuctionDatum{..}) <- findAuction bpCurrency bpToken
    logInfo @P.String $ printf "found auction utxo with datum %s" (P.show d)

    when (bpBid < minBid d) $                             {--проверка на то, что размер ставки больше минимума(=прошлого)--}
        throwError $ pack $ printf "bid lower than minimal bid %d" (minBid d)
    pkh <- ownPaymentPubKeyHash
    let b  = Bid {bBidder = pkh, bBid = bpBid}  
        d' = d {adHighestBid = Just b}                     {--новая datum, потом обновится в самом аукционе(=обновление минимальной ставки)--}
        v  = Value.singleton bpCurrency bpToken 1 <> Ada.lovelaceValueOf (minLovelace + bpBid)  {--прибавление ставки к скрипту--}
        r  = Redeemer $ PlutusTx.toBuiltinData $ MkBid b

        lookups = Constraints.typedValidatorLookups typedAuctionValidator P.<>
                  Constraints.otherScript auctionValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = case adHighestBid of
                    Nothing      -> Constraints.mustPayToTheScript d' v                            <> {--locks the value v with a script alongside a datum d--}
                                    Constraints.mustValidateIn (to $ aDeadline adAuction)          <> {--транзакция должна быть завершена до дедлайна--}
                                    Constraints.mustSpendScriptOutput oref r                          {-- It creates an input to this script address. As parameters it takes the reference to the UTxO we want to consume, and it takes a Redeemer--}
                    Just Bid{..} -> Constraints.mustPayToTheScript d' v                            <>
                                    Constraints.mustPayToPubKey bBidder (Ada.lovelaceValueOf bBid) <>
                                    Constraints.mustValidateIn (to $ aDeadline adAuction)          <>
                                    Constraints.mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "made bid of %d lovelace in auction %s for token (%s, %s)"
        bpBid
        (P.show adAuction)
        (P.show bpCurrency)
        (P.show bpToken)

close :: forall w s. CloseParams -> Contract w s Text ()
close CloseParams{..} = do
    (oref, o, d@AuctionDatum{..}) <- findAuction cpCurrency cpToken
    logInfo @P.String $ printf "found auction utxo with datum %s" (P.show d)

    let t      = Value.singleton cpCurrency cpToken 1
        r      = Redeemer $ PlutusTx.toBuiltinData Close
        seller = aSeller adAuction

        lookups = Constraints.typedValidatorLookups typedAuctionValidator P.<>
                  Constraints.otherScript auctionValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = case adHighestBid of
                    Nothing      -> Constraints.mustPayToPubKey seller (t <> Ada.lovelaceValueOf minLovelace)  <>
                                    Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                    Constraints.mustSpendScriptOutput oref r
                    Just Bid{..} -> Constraints.mustPayToPubKey bBidder (t <> Ada.lovelaceValueOf minLovelace) <>
                                    Constraints.mustPayToPubKey seller (Ada.lovelaceValueOf bBid)              <>
                                    Constraints.mustValidateIn (from $ aDeadline adAuction)                    <>
                                    Constraints.mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "closed auction %s for token (%s, %s)"
        (P.show adAuction)
        (P.show cpCurrency)
        (P.show cpToken)

findAuction :: CurrencySymbol
            -> TokenName
            -> Contract w s Text (TxOutRef, ChainIndexTxOut, AuctionDatum)
findAuction cs tn = do
    utxos <- utxosAt $ scriptHashAddress auctionHash
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             , Value.valueOf (_ciTxOutValue o) cs tn == 1
             ]
    case xs of
        [(oref, o)] -> case _ciTxOutDatum o of
            Left _          -> throwError "datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "datum has wrong type"
                Just d@AuctionDatum{..}
                    | aCurrency adAuction == cs && aToken adAuction == tn -> return (oref, o, d)
                    | otherwise                                           -> throwError "auction token missmatch"
        _           -> throwError "auction utxo not found"

endpoints :: Contract () AuctionSchema Text ()
endpoints = awaitPromise (start' `select` bid' `select` close') >> endpoints
  where
    start' = endpoint @"start" start
    bid'   = endpoint @"bid"   bid
    close' = endpoint @"close" close

mkSchemaDefinitions ''AuctionSchema {--helpers to create a sample NFT in the playground--}

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

mkKnownCurrencies ['myToken]