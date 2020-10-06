{-# LANGUAGE QuasiQuotes #-}
module HaskoinSpec (spec) where

import Protolude
import Test.Hspec
import Data.Aeson.QQ
import Data.Aeson (Value)
import BlockchainsQueryApi
import BlockchainsQueryApi.Haskoin (parseTx)

spec :: Spec
spec =
    describe "Haskoin" $
        describe "parseTx" $ do
            it "converts an empty result to nothing" $
                parseTx emptyResult `shouldBe` Nothing
            it "converts a valid transaction value to a proper Tx" $
                parseTx validTransaction `shouldBe` Just expectedTransaction

            where
                expectedTransaction = Tx
                                        { txHash = "eba91c9d93abcc2369471ee5bb9d92054f9e38ab61801092c4302bd2a6f9befe"
                                        , txFee = 24716
                                        , txFrom = [Balance "1FoWyxwPXuj4C6abqwhjDWdz6D4PZgYRjA" 2770424]
                                        , txTo = [Balance "1FoWyxwPXuj4C6abqwhjDWdz6D4PZgYRjA" 2745162, Balance "19bsk5m9PX2PcgrUaSCfMSzRZgGo4d9vgH" 546]
                                        }
                validTransaction :: Value
                validTransaction = [aesonQQ| 
                                    {
                                        "time": 1601843602,
                                        "size": 256,
                                        "inputs": [
                                            {
                                                "pkscript": "76a914a25dec4d0011064ef106a983c39c7a540699f22088ac",
                                                "value": 2770424,
                                                "address": "1FoWyxwPXuj4C6abqwhjDWdz6D4PZgYRjA",
                                                "witness": null,
                                                "sequence": 4294967294,
                                                "output": 0,
                                                "sigscript": "47304402204b914fa791c610625d16dbbb15e74d1fa30b075c2417202053e305fec548c3cd022031ffb306b1965626acbb48cfd441d53b032b4ebd516944793563e4ce4baeb306012103da5bac7b36d5aa38f531c6b9601e21bb598a4b6716ebed38b009a55dabde9440",
                                                "coinbase": false,
                                                "txid": "b4590c61b550951cd055a9429dc81d57267ba53aa7503f2107ca1e0e2eb15270"
                                            }
                                        ],
                                        "weight": 1024,
                                        "fee": 24716,
                                        "locktime": 651202,
                                        "block": {
                                            "height": 651278,
                                            "position": 2807
                                        },
                                        "outputs": [
                                            {
                                                "spent": false,
                                                "pkscript": "6a146f6d6e69000000000000001f00000001f907b840",
                                                "value": 0,
                                                "address": null,
                                                "spender": null
                                            },
                                            {
                                                "spent": false,
                                                "pkscript": "76a914a25dec4d0011064ef106a983c39c7a540699f22088ac",
                                                "value": 2745162,
                                                "address": "1FoWyxwPXuj4C6abqwhjDWdz6D4PZgYRjA",
                                                "spender": null
                                            },
                                            {
                                                "spent": true,
                                                "pkscript": "76a9145e5981c5b298306353d1725c863337a1ce6e95a288ac",
                                                "value": 546,
                                                "address": "19bsk5m9PX2PcgrUaSCfMSzRZgGo4d9vgH",
                                                "spender": {
                                                    "input": 169,
                                                    "txid": "a9bc3e05e7cd1703401e9cc4d0743d3a957f454dc635cb97dc866baf12475e5f"
                                                }
                                            }
                                        ],
                                        "version": 2,
                                        "deleted": false,
                                        "rbf": false,
                                        "txid": "eba91c9d93abcc2369471ee5bb9d92054f9e38ab61801092c4302bd2a6f9befe"
                                    }
                |]
                emptyResult :: Value
                emptyResult = [aesonQQ| {"error":"not-found"} |]