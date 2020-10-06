{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module BlockchainsQueryApi.Parity
    ( parity
    -- exported for test only
    , parseTx
    ) where

import BlockchainsQueryApi.Prelude
import BlockchainsQueryApi.Domain
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson.Lens
import Data.Aeson
import Control.Lens ((^?), preview)
import Network.URI
import Network.HTTP.Simple
import Conduit (MonadThrow)
import Data.Aeson.QQ

data ParityRequest =
    ParityRequest
        { trmethod :: Text
        , trparams :: Value
        , trid :: Integer
        , trjsonrpc :: Text
        } deriving (Eq, Show)

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 2 } ''ParityRequest)

parity :: (MonadIO m,  MonadThrow m) => URI -> Node m
parity uri = Node 
                { nodeTransaction = getTransaction uri
                , nodeBalance = getBalance uri
                , nodeCurrentBlock = getCurrentBlock uri
                , nodeFeeEstimate = getFeeEstimate uri
                }

getFeeEstimate :: (MonadIO m,  MonadThrow m) => URI -> m (Either Error FeeEstimate)
getFeeEstimate uri =
    responseOrError <$> mkRequest uri "eth_gasPrice" [aesonQQ| [] |]
    where
        responseOrError = either (Left . RPCError . show) blockOrFailure
        blockOrFailure :: Value -> Either Error FeeEstimate
        blockOrFailure body =
            maybeToEither (RPCError $ toS $ "Error decoding: " <> encode body) (responseToBlock body)

        responseToBlock :: Value -> Maybe FeeEstimate
        responseToBlock body = do
            rGasPrice <- body  ^? key "result" . _String
            rGasPriceNumeric <- fromIntegral <$> (readMaybe $ toS rGasPrice :: Maybe Integer)
            pure $ FeeEstimate ETH rGasPriceNumeric

getCurrentBlock :: (MonadIO m,  MonadThrow m) => URI -> m (Either Error Block)
getCurrentBlock uri =
    responseOrError <$> mkRequest uri "eth_getBlockByNumber" [aesonQQ| ["latest", true] |]
    where
        responseOrError = either (Left . RPCError . show) blockOrFailure
        blockOrFailure :: Value -> Either Error Block
        blockOrFailure body =
            maybeToEither (RPCError $ toS $ "Error decoding: " <> encode body) (responseToBlock body)

        responseToBlock :: Value -> Maybe Block
        responseToBlock body = do
            rId <- body  ^? key "result" . key "hash" . _String
            rNumber <- body  ^? key "result" . key "number" . _String
            rNumberNumeric <- fromIntegral <$> (readMaybe $ toS rNumber :: Maybe Integer)
            rTransactionValues <- body  ^? key "result" . key "transactions" . _Array
            let transactions = mapMaybe (rightToMaybe . parseTx) $ toList rTransactionValues
            pure $ Block rId rNumberNumeric transactions

getBalance :: (MonadIO m,  MonadThrow m) => URI -> Address -> m (Either Error Balance)
getBalance uri address =
    responseOrError <$> mkRequest uri "eth_getBalance" [aesonQQ| [#{address}, "pending"] |]
    where
        responseOrError = either (Left . RPCError . show) balanceOrFailure
        balanceOrFailure :: Value -> Either Error Balance
        balanceOrFailure body =
            maybeToEither (RPCError $ toS $ "Error decoding: " <> encode body) (responseToBalance body)

        responseToBalance :: Value -> Maybe Balance
        responseToBalance body = do
            rBalance <- body ^? key "result" . _String
            rBalanceNumeric <- fromIntegral <$> (readMaybe $ toS rBalance :: Maybe Integer)
            pure $ Balance address rBalanceNumeric

getTransaction :: (MonadIO m,  MonadThrow m) => URI -> Text -> m (Either Error Tx)
getTransaction uri paramHash =
    responseOrError <$> mkRequest uri "eth_getTransactionByHash" [aesonQQ| [#{paramHash}] |]
    where
        responseOrError =
            either 
            (const $ Left $ NotFound ("Could not find the hash " <> paramHash <> " in parity")) 
            txOrFailure 

        txOrFailure :: Value -> Either Error Tx
        txOrFailure = successfulResponse >=> parseTx
        
mkRequest :: (MonadIO m,  MonadThrow m) => URI -> Text -> Value -> m (Either Error Value)
mkRequest uri method params = do 
    jsonRequest <- setRequestBodyJSON parityRequest <$> parseRequest uri'
    responseOrError <- httpJSONEither jsonRequest
    pure $ getResponseBody $ mapError <$> responseOrError
    where
        mapError = mapLeft (const $ RPCError "Error decoding response")
        uri' = "POST " <> uriToString identity uri ""
        parityRequest :: ParityRequest
        parityRequest = ParityRequest method params 2 "2.0"

successfulResponse :: Value -> Either Error Value
successfulResponse = maybeToEither (NotFound "Has no result key") . hasResult
    where
        hasResult  :: Value -> Maybe Value
        hasResult = preview $ key "result"

parseTx :: Value -> Either Error Tx
parseTx = maybeToEither (NotFound "Error decoding Tx") . hasTx
    where 
        hasTx :: Value -> Maybe Tx
        hasTx body = do
            rTxId <- body ^? key "hash" . _String
            rFrom <- body ^? key "from" . _String
            rTo <- body ^? key "to" . _String

            rGasPrice <- body ^? key "gasPrice" . _String
            rGasPriceNumeric <- fromIntegral <$> (readMaybe $ toS rGasPrice :: Maybe Integer)
            rGas <- body ^? key "gas" . _String
            rGasNumeric <- fromIntegral <$> (readMaybe $ toS rGas :: Maybe Integer)
            rValue <- body ^? key "value" . _String
            rValueNumeric <- fromIntegral <$> (readMaybe $ toS rValue :: Maybe Integer)

            let fee = rGasPriceNumeric * rGasNumeric
            pure Tx
                { txHash = rTxId
                , txFee = fee
                , txFrom = [Balance rFrom (rValueNumeric + fee)]
                , txTo = [Balance rTo rValueNumeric]
                }
