{-# LANGUAGE QuasiQuotes #-}
module ETHApiSpec (spec) where

import Protolude hiding (get)

import BlockchainsQueryApi hiding (get)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.URI
import Control.Lens ((.~))

-- Basic imports
import qualified Network.Ethereum.Api.Web3 as Eth

-- Eth API support
import qualified Network.Ethereum.Api.Eth   as Eth
import qualified Network.Ethereum.Api.Types as Eth
import qualified Network.Ethereum           as Eth
import qualified Network.Web3               as Eth
import Crypto.Ethereum (importKey)
import Data.ByteArray.HexString (HexString, toText)

import Unsafe (unsafeHead)

spec :: Spec
spec =
    with (mkServer <$> mkContext conf) $ do
            describe "GET /:currency/balance/:address" $ do
                it "responds with 200 when given address is found" $
                    get "/eth/balance/0x00a329c0648769a73afac7f9381e08fb43dbea72" `shouldRespondWith` 200
                it "responds with 0 Balance when given address is not found" $
                    get "/eth/balance/0x00a329c0648769a73afac7f9381e08fb43dbea71" `shouldRespondWith` [json|{"address":"0x00a329c0648769a73afac7f9381e08fb43dbea71","value":0}|]

            describe "GET /:currency/tx/:hash" $ do
                it "responds with 404 when given hash is not a valid transaction hash" $
                    get "/eth/tx/invalid-hash" `shouldRespondWith` 404
                it "responds with 404 when given hash is not found" $
                    get "/eth/tx/0xad54c43d670eb9c5758fdfc9ce508899123af791042206fd619ccc0ac07c37dd" `shouldRespondWith` 404
                it "responds with 200 and proper data when given hash is not found" $ do
                    ret <- Eth.runWeb3 $ do
                        -- Get address of default account
                        me <- unsafeHead <$> Eth.accounts
                        -- Use default account
                        Eth.withAccount (Eth.LocalKey (importKey privateKey) devChain) $
                            -- Send transaction with value
                            Eth.withParam (Eth.value .~ (1 :: Eth.Wei)) $
                                    Eth.withParam (Eth.to .~ me) $ Eth.send ()
                    let testTxHash = toText $ Eth.receiptTransactionHash (either (panic "Error setting up eth test") identity ret)
                    get ("/eth/tx/" <> toS testTxHash) `shouldRespondWith` 200
            describe "GET /:currency/fee-estimate" $
                it "responds with 200 and block information" $
                    get "/eth/fee-estimate" `shouldRespondWith` 200
            describe "GET /:currency/current-block" $
                it "responds with 200 and block information" $
                    get "/eth/current-block" `shouldRespondWith` 200

    where
        devChain = 17 :: Integer
        privateKey = "0x4d5db4107d237df6a3d58ee5f70ae63d73d7658d4026f2eefd2f204c81682cb7" :: HexString
        conf = BTConfig 
                { port = 8080
                , haskoinBtcUri = parseURI "https://btc.haskoin.com/api"
                , haskoinBchUri = parseURI "https://btc.haskoin.com/api"
                , parityUri = parseURI "http://localhost:8545"
                }
