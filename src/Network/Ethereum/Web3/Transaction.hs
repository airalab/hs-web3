module Network.Ethereum.Web3.Transaction where

import           Crypto.Hash                       (Digest, Keccak_256, hash)
import           Crypto.Secp256k1
import           Data.ByteArray                    (Bytes, convert)
import           Data.ByteString
import qualified Data.ByteString                   as BS
import           Data.ByteString.Short             (fromShort)
import qualified Data.ByteString.Base16            as BS16
import           Data.Maybe                        (fromMaybe)
import           Data.RLP
import           Network.Ethereum.Web3.Types
import           Network.Ethereum.ABI.Prim.Address (toHexString)

unpackCallParameters :: Call -> Maybe (Integer, Integer, Integer, ByteString, Integer, ByteString)
unpackCallParameters call = (,,,,,) <$> (unQuantity <$> callNonce call)
                                    <*> (unQuantity <$> callGasPrice call)
                                    <*> (unQuantity <$> callGas call)
                                    <*> (fst . BS16.decode . BS.drop 2 . toHexString <$> callTo call)
                                    <*> (unQuantity <$> callValue call)
                                    <*> (pure . fromMaybe empty $ convert <$> callData call)

-- initial rlpHash of transaction data
rlpHashTransaction :: Call -> Integer -> Maybe ByteString
rlpHashTransaction call chainId = do
    (nonce, gasPrice, gasLimit, to, value, txData) <- unpackCallParameters call
    return . packRLP $ rlpEncode ( nonce
                                 , gasPrice
                                 , gasLimit
                                 , to
                                 , value
                                 , txData
                                 , chainId
                                 , 0 :: Integer
                                 , 0 :: Integer
                                 )


createRawTransaction :: Call -> Integer -> ByteString -> Maybe Bytes
createRawTransaction call chainId privateKey = do
    rlpEndcoding <- rlpHashTransaction call chainId
    let rlpHash = convert (hash rlpEndcoding :: Digest Keccak_256)
    (nonce, gasPrice, gasLimit, to, value, txData) <- unpackCallParameters call
    recSig <- ecsign rlpHash privateKey
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
