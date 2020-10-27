{-# LANGUAGE QuasiQuotes #-}
module XRPApiSpec (spec) where

import Protolude hiding (get)

import BlockchainsQueryApi hiding (get)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.URI
import Control.Lens ((.~))

-- Basic imports
import qualified Network.Web3               as Eth

-- Eth API support
import qualified Network.Ethereum.Api.Eth   as Eth
import qualified Network.Ethereum.Api.Types as Eth
import Crypto.Ethereum (importKey)
import Data.ByteArray.HexString (HexString, toText)

import Unsafe (unsafeHead)

spec :: Spec
spec =
    with (mkServer <$> mkContext conf) $
            describe "GET /:currency/balance/:address" $ do
                it "responds with 200 when given address is found" $
                    get "/xrp/balance/rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh" `shouldRespondWith` 200
                it "responds with 0 Balance when given address is not found" $
                    get "/xrp/balance/rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh" `shouldRespondWith` [json|{"address":"rHb9CJAWyB4rj91VRWn96DkukG4bwdtyTh","value":100000000000000000}|]
    where
        conf = BTConfig 
                { port = 8080
                , haskoinBtcUri = parseURI "https://btc.haskoin.com/api"
                , haskoinBchUri = parseURI "https://btc.haskoin.com/api"
                , parityUri = parseURI "http://localhost:8545"
                , rippleUri = parseURI "http://localhost:5005"
                }
