module BlockchainsQueryApi.Haskoin
    ( haskoin
    -- exported for test only
    , parseTx
    ) where

import BlockchainsQueryApi.Prelude
import BlockchainsQueryApi.Domain
import Data.Aeson.Lens
import Data.Aeson
import Control.Lens ((^?), (^..))
import Network.URI
import Network.HTTP.Simple
import Conduit (MonadThrow)
import Data.String (String)

haskoin :: (MonadIO m,  MonadThrow m) => URI -> Node m
haskoin uri = Node 
                { nodeTransaction = getTransaction uri
                , nodeBalance = getBalance uri
                , nodeCurrentBlock = getCurrentBlock uri
                , nodeFeeEstimate = getFeeEstimate uri
                }

getFeeEstimate :: (MonadIO m,  MonadThrow m) => URI -> m (Either Error FeeEstimate)
getFeeEstimate uri = do
    (status, jsonBody) <- mkRequest uriToString'
    pure $ case status of
        200 -> maybeToEither (RPCError "Invalid block body") (responseToBlock jsonBody)
        _ -> Left $ RPCError $ "Some other error, haskoin response status: " <> show status
    where
        responseToBlock :: Value -> Maybe FeeEstimate
        responseToBlock body = do
            rSize <- fromInteger <$> body ^? key "size" . _Integer
            rFees <- fromInteger <$> body ^? key "fees" . _Integer
            pure $ FeeEstimate BTC (rFees / rSize)
        uriToString' = "GET " <> uriToString identity uri "" <> "/block/best"

getCurrentBlock :: (MonadIO m,  MonadThrow m) => URI -> m (Either Error Block)
getCurrentBlock uri = do
    (status, jsonBody) <- mkRequest uriToString'
    txsAttempts <- mapM (getTransaction uri) (txIds jsonBody)
    pure $ case (status, sequence txsAttempts) of
        (200, Right txs) -> maybeToEither (RPCError "Invalid block body") (responseToBlock jsonBody txs)
        _ -> Left $ RPCError $ "Some other error, haskoin response status: " <> show status
    where
        txIds :: Value -> [Text]
        txIds body = 
            body ^.. key "tx" . values . _String
        responseToBlock :: Value -> [Tx] -> Maybe Block
        responseToBlock body txs' = do
            rId <- body ^? key "hash" . _String
            rNumber <- body ^? key "height" . _Integer
            pure $ Block rId rNumber txs'

        uriToString' = "GET " <> uriToString identity uri "" <> "/block/best"

getBalance :: (MonadIO m,  MonadThrow m) => URI -> Address -> m (Either Error Balance)
getBalance uri address = do
    (status, jsonBody) <- mkRequest uriToString'
    pure $ case status of
        200 -> maybeToEither (RPCError "Invalid balance body") (responseToBalance jsonBody)
        404 -> Left $ NotFound ("Could not find the address " <> toS address <> " in Haskoin")
        _ -> Left $ RPCError $ "Some other error, haskoin response status: " <> show status
    where
        uriToString' = "GET " <> uriToString identity uri "" <> "/address/" <> toS address <> "/balance"

responseToBalance :: Value -> Maybe Balance
responseToBalance body = do
    rAddress <- body  ^? key "address" . _String
    rConfirmed <- body  ^? key "confirmed" . _Integer
    rUnconfirmed <- body  ^? key "unconfirmed" . _Integer
    pure $ Balance rAddress (rConfirmed + rUnconfirmed)

getTransaction :: (MonadIO m,  MonadThrow m) => URI -> Text -> m (Either Error Tx)
getTransaction uri paramHash = do
    (status, jsonBody) <- mkRequest uriToString'
    pure $ case status of
        200 -> maybeToEither (RPCError "Invalid transaction body") (parseTx jsonBody)
        404 -> Left $ NotFound ("Could not find the hash " <> toS paramHash <> " in Haskoin")
        _ -> Left $ RPCError $ "Some other error, haskoin response status: " <> show status
    where
        uriToString' = "GET " <> uriToString identity uri "" <> "/transaction/" <> toS paramHash

mkRequest :: (MonadIO m,  MonadThrow m) => String -> m (Int, Value)
mkRequest uri = 
    parseRequest uri 
    >>= httpJSON 
    >>= (\r -> pure (getResponseStatusCode r, getResponseBody r))

parseTx :: Value -> Maybe Tx
parseTx body = do
    rTxId <- body  ^? key "txid" . _String
    rFee <- body  ^? key "fee" . _Integer
    let
        rFromAddresses = body ^.. key "inputs" . values . key "address" . _String
        rFromValues = body ^.. key "inputs" . values . key "value" . _Integer
        rOutputs = body ^.. key "outputs" . values
    pure Tx
        { txHash = rTxId
        , txFee = rFee
        , txFrom = uncurry Balance <$> zip rFromAddresses rFromValues
        , txTo = mapMaybe parseBalance rOutputs
        }
    where
        parseBalance :: Value -> Maybe Balance
        parseBalance balanceBody = do
            rAddress <- balanceBody  ^? key "address" . _String
            rValue <- balanceBody  ^? key "value" . _Integer
            pure $ Balance rAddress rValue
