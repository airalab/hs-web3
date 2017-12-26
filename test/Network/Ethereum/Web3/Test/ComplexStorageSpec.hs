{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Ethereum.Web3.Test.ComplexStorageSpec where

import           Data.ByteArray                   (convert)
import           Data.ByteString                  (ByteString)
import           Data.Default
import           Data.Either                      (isRight)
import           Data.String                      (fromString)
import           Network.Ethereum.Web3            hiding (convert)
import qualified Network.Ethereum.Web3.Eth        as Eth
import           Network.Ethereum.Web3.Test.Utils
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types      (Call (..))
import           System.Environment               (getEnv)
import           System.IO.Unsafe                 (unsafePerformIO)
import           Test.Hspec

[abiFrom|build/contracts/abis/ComplexStorage.json|]

spec :: Spec
spec = before withPrimaryEthereumAccount $
    describe "can interact with a ComplexStorage contract" $ do
        -- todo: these should ideally be arbitrary!
        let sUint   = 1
            sInt    = 1
            sBool   = True
            sInt224 = 221
            sBools   = [True, False]
            sInts    = [1, 1, 3]
            sString  = "hello"
            sBytes16 = BytesN $ convert ("\x12\x34\x56\x78\x12\x34\x56\x78\x12\x34\x56\x78\x12\x34\x56\x78" :: ByteString)
            sByte2s  = replicate 4 (BytesN (convert ("\x12\x34" :: ByteString)))
            contractAddress = fromString . unsafePerformIO $ getEnv "COMPLEXSTORAGE_CONTRACT_ADDRESS"
        it "should inject contract addresses" (const injectExportedEnvironmentVariables)
        it "can set the values of a ComplexStorage" $ \primaryAccount -> do
            let theCall = callFromTo primaryAccount contractAddress
            ret <- runWeb3Configured $ setValues theCall
                                                 sUint
                                                 sInt
                                                 sBool
                                                 sInt224
                                                 sBools
                                                 sInts
                                                 sString
                                                 sBytes16
                                                 sByte2s
            True `shouldBe` True -- we need to get this far :)

        it "can verify that it set the values correctly" $ \primaryAccount -> do
            let theCall = callFromTo primaryAccount contractAddress
                runGetterCall f = runWeb3Configured (f theCall)
            -- gotta sleep for the block to get confirmed!
            sleepSeconds 5
            -- there really has to be a better way to do this
            -- also need to figure out how to do vectors completely...
            uintVal'    <- runGetterCall uintVal
            intVal'     <- runGetterCall intVal
            boolVal'    <- runGetterCall boolVal
            int224Val'  <- runGetterCall int224Val
            --boolsVal    <- runGetterCall boolVectorVal
            --intsVal     <- runGetterCall intListVal
            stringVal'  <- runGetterCall stringVal
            bytes16Val' <- runGetterCall bytes16Val
            --bytes2s     <- runGetterCall bytes2VectorListVal
            uintVal'    `shouldBe` sUint
            intVal'     `shouldBe` sInt
            boolVal'    `shouldBe` sBool
            int224Val'  `shouldBe` sInt224
            --boolsVal    `shouldBe` sBools
            --intsVal     `shouldBe` sInts
            stringVal'  `shouldBe` sString
            bytes16Val' `shouldBe` sBytes16
            -- bytes2s `shouldBe` sByte2s
