module Network.Ethereum.Web3.Transaction where

import           Crypto.Hash                       (Digest, Keccak_256, hash)
import           Crypto.Secp256k1
import           Data.ByteArray                    (Bytes, convert)
import           Data.ByteArray.Encoding
import           Data.ByteString
import qualified Data.ByteString                   as BS
import           Data.ByteString.Short             (fromShort)
import           Data.Maybe                        (fromMaybe)
import           Data.RLP
import           Network.Ethereum.ABI.Prim.Address (toHexString)
import           Network.Ethereum.Web3.Types

unpackCallParameters :: Call -> Either String (Integer, Integer, Integer, ByteString, Integer, ByteString)
unpackCallParameters call = do
    nonce <- toError "nonce" $ unQuantity <$> callNonce call
    gasPrice <- toError "gasPrice" $ unQuantity <$> callGasPrice call
    gasLimit <- toError "gas" $ unQuantity <$> callGas call
    to <- convertFromBase Base16 =<< toError "to" (BS.drop 2 . toHexString <$> callTo call)
    value <- toError "value" (unQuantity <$> callValue call)
    let txData = fromMaybe empty $ convert <$> callData call
    return (nonce, gasPrice, gasLimit, to, value, txData)
        where toError field =
                maybe (Left $ field ++ " must be set when creating raw transaction") pure

createRawTransaction :: Call -> Integer -> ByteString -> Either String Bytes
createRawTransaction call chainId privateKey = do
    (nonce, gasPrice, gasLimit, to, value, txData) <- unpackCallParameters call
    let rlpEndcoding =  packRLP $ rlpEncode ( nonce
                                            , gasPrice
                                            , gasLimit
                                            , to
                                            , value
                                            , txData
                                            , chainId
                                            , 0 :: Integer
                                            , 0 :: Integer
                                            )
        rlpHash = convert (hash rlpEndcoding :: Digest Keccak_256)
    recSig <- maybe (Left "Cannot create transaction signature") pure $ ecsign rlpHash privateKey
    let r = fromShort $ getCompactRecSigR recSig
        s = fromShort $ getCompactRecSigS recSig
        v = getCompactRecSigV recSig
    return . convert . packRLP $
        rlpEncode ( nonce
                  , gasPrice
                  , gasLimit
                  , to
                  , value
                  , txData
                  , fromIntegral v + 27 + chainId * 2 + 8
                  , s -- swapping s & r, still don't know why
                  , r
                  )


ecsign :: ByteString -> ByteString -> Maybe CompactRecSig
ecsign msgHash privateKey = do
    msgHash' <- msg msgHash
    privateKey' <- secKey privateKey
    return . exportCompactRecSig $ signRecMsg privateKey' msgHash'
