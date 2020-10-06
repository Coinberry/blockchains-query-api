{-# LANGUAGE TemplateHaskell #-}

module BlockchainsQueryApi.Ripple
    ( ripple
    ) where

import BlockchainsQueryApi.Prelude

import BlockchainsQueryApi.Domain

import Data.Aeson.Lens

import Control.Lens ((^?))

import Network.URI

import Network.HTTP.Simple

import Conduit (MonadThrow)

import Data.String (String)

import Data.Aeson (Value (..), encode)

import Data.Aeson.TH (deriveJSON, defaultOptions)

data RippleRequest a =
  RippleRequest
      { jsonrpc :: String
      , id :: String
      , method :: String
      , params :: [a]
      } deriving (Eq, Show)

$(deriveJSON defaultOptions ''RippleRequest )

data RippleParams =
    RippleParams
    {  account ::  String
    ,  ledger_index :: String
    ,  strict :: Bool
    ,  queue :: Bool
    } deriving (Eq, Show)

$(deriveJSON defaultOptions ''RippleParams )

ripple :: (MonadIO m,  MonadThrow m) => URI -> Node m
ripple uri = Node 
                { nodeTransaction = panic "Transaction not implmented for Ripple nodes"
                , nodeBalance = getBalance uri
                , nodeCurrentBlock = panic "Current Block not implmented for Ripple nodes"
                , nodeFeeEstimate = panic "Fee Estimates not implmented for Ripple nodes"
                }


getBalance :: (MonadIO m, MonadThrow m) => URI -> Address -> m (Either Error Balance)
getBalance uri address = do
    request <- mkRequest
    let jsonRequest = setRequestBodyJSON body request
        requestWithHeader = setRequestHeader "Content-Type" ["application/json"] jsonRequest
    response <- httpJSON requestWithHeader
    let jsonBody = getResponseBody response :: Value
    pure $ balanceOrFailure jsonBody
    where
        mkRequest = parseRequest uriToString'
        uriToString' = "POST " <> uriToString identity uri ""
        body = buildBalanceRequest address

        balanceOrFailure :: Value -> Either Error Balance

        balanceOrFailure balanceBody =
            maybeToEither (RPCError $ toS $ "Error decoding: " <> encode balanceBody) (responseToBalance balanceBody)

responseToBalance :: Value -> Maybe Balance
responseToBalance body = do 
    account_data <- body ^? key "result" . key "account_data"
    balance <- account_data ^? key "Balance" . _String
    rippleAccount <- account_data ^? key "Account" . _String
    rBalanceNumeric <- fromIntegral <$> (readMaybe $ toS balance :: Maybe Integer)
    pure $ Balance rippleAccount rBalanceNumeric

buildBalanceRequest :: Address -> RippleRequest RippleParams
buildBalanceRequest address =  
    RippleRequest 
        { jsonrpc = "2.0"
        , id = "1"
        , method = "account_info"
        , params = [
                         
                        RippleParams
                        { account = toS address
                        , strict =  True
                        , ledger_index = "current"
                        , queue = True
                        }
                    ]
        }



