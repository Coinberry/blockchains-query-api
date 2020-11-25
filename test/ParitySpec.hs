{-# LANGUAGE QuasiQuotes #-}
module ParitySpec (spec) where

import Protolude
import Test.Hspec
import Data.Aeson.QQ
import Data.Aeson (Value)
import BlockchainsQueryApi
import BlockchainsQueryApi.Parity

spec :: Spec
spec =
    describe "Parity" $
        describe "parseTx" $ do
            it "converts an empty result to nothing" $
                parseTx emptyResult `shouldBe` Left (NotFound "Error decoding Tx")
            it "converts a valid transaction value to a Tx" $
                parseTx validTransaction `shouldSatisfy` isRight
            where
                validTransaction :: Value
                validTransaction = [aesonQQ| {"raw":"0xf86e820598850649534e0082520894ef7077fec58dd651e0af96ef238e5f7aa5bfdb308820c7e687ae65e000801ba088ad224e0b969c6f93d9456f3e79e1128185bdb1487469ee8ea14a58c4c4393ea0639c7d4fc21fbd987036c383e12aad69d92626c085d9d2cabf51a5919b037ed0","creates":null,"hash":"0x64bc5c9f4f4dde937d3d9e56f391c49171f1e8ca1f313c5773f7a66818a17739","gas":"0x5208","chainId":null,"gasPrice":"0x649534e00","to":"0xef7077fec58dd651e0af96ef238e5f7aa5bfdb30","value":"0x20c7e687ae65e000","blockHash":"0x6ebfdef78a287b4df884fa9904db9436e954aed62d034ba1cd263f01db7955cc","input":"0x","from":"0x628e4b908d851d53b9b1c0b4355eec4a694483de","publicKey":"0x52e7e43b55af0a2ea888a2f6d1ebafb9614a43b8473940e52f82b81900b3559338183b158f578fa5af4b4364e31dba9f61cc9c8c517bc79b1f89c5d9f25d6403","blockNumber":"0x878859","transactionIndex":"0x1c","r":"0x88ad224e0b969c6f93d9456f3e79e1128185bdb1487469ee8ea14a58c4c4393e","standardV":"0x0","s":"0x639c7d4fc21fbd987036c383e12aad69d92626c085d9d2cabf51a5919b037ed0","condition":null,"v":"0x1b","nonce":"0x598"} |]
                emptyResult :: Value
                emptyResult = [aesonQQ| {"result":null,"jsonrpc":"2.0","id":"jsonrpc"} |]