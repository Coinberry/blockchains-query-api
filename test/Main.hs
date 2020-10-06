module Main where

import Protolude

import Test.Hspec.Runner
import System.Process

import qualified BTCApiSpec as BTC
import qualified ETHApiSpec as ETH
import qualified XRPApiSpec as XRP
import qualified ParitySpec as Parity
import qualified HaskoinSpec as Haskoin

import qualified RippleSpec as Ripple

main :: IO ()
main = do
    withParity $
        hspecWith defaultConfig ETH.spec

    withRippled $
        hspecWith defaultConfig $ XRP.spec >> Ripple.spec

    hspecWith defaultConfig specsWithoutMockServer    
    where
        specsWithoutMockServer = Parity.spec >> Haskoin.spec

withParity :: IO () -> IO ()
withParity action = 
    withCreateProcess
        (proc "parity" ["-c", "dev-insecure"])
        (\_ _ _ _ -> threadDelay 10000000 >> action)

withRippled :: IO () -> IO ()
withRippled action = catch withRippledNative withRippledManualStart
    where
        withRippledManualStart :: SomeException -> IO ()
        withRippledManualStart _ = putStrLn ("============================================================\n\nPlease start a Rippled in stand alone mode for the XRP specs\n\n============================================================" :: Text)
        withRippledNative =
            withCreateProcess
                (proc "rippled" ["-a", "--conf", "./test/rippled.conf"])
                (\_ _ _ _ -> threadDelay 10000000 >> action)