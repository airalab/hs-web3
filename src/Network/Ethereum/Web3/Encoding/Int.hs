{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Web3.Encoding.Int where

import qualified Data.ByteString as BS
import Data.Proxy (Proxy(..))
import Network.Ethereum.Web3.Encoding(ABIEncode(..), ABIDecode(..))
import Network.Ethereum.Web3.Encoding.Internal (EncodingType(..), int256HexBuilder, int256HexParser)
import GHC.TypeLits


-- | Sized unsigned integers
newtype UIntN (n :: Nat) =
  UIntN { unUIntN :: Integer } deriving (Eq, Show, Enum, Ord, Real, Integral, Num)

uIntNFromInteger :: forall n . KnownNat n => Integer -> Maybe (UIntN n)
uIntNFromInteger a
  | a < 0 = Nothing
  | otherwise = let maxVal = 2 ^ (natVal (Proxy :: Proxy n)) - 1
                in if a > maxVal then Nothing else Just . UIntN $ a

instance ABIEncode (UIntN n) where
  toDataBuilder = toDataBuilder . unUIntN

instance KnownNat n => ABIDecode (UIntN n) where
  fromDataParser = do
    a <- int256HexParser
    case uIntNFromInteger a :: Maybe (UIntN n) of
      Nothing -> fail $ "Could not parse as " ++ typeName (Proxy :: Proxy (UIntN n)) ++ ": " ++ show a
      Just a' -> return a'

instance KnownNat n => EncodingType (UIntN n) where
  typeName = let n = show . natVal $ (Proxy :: Proxy n)
             in const $ "int" ++ n
  isDynamic = const False

-- | Sized signed integers
newtype IntN (n :: Nat) =
  IntN { unIntN :: Integer } deriving (Eq, Show, Enum, Ord, Real, Integral, Num)

intNFromInteger :: forall n . KnownNat n => Integer -> Maybe (IntN n)
intNFromInteger a
  | a < 0 = let minVal = negate $ 2 ^ (natVal (Proxy :: Proxy n) - 1)
            in if a < minVal then Nothing else Just . IntN $ a
  | otherwise = let maxVal = 2 ^ (natVal (Proxy :: Proxy n) - 1) - 1
                in if a > maxVal then Nothing else Just . IntN $ a

instance KnownNat n => EncodingType (IntN n) where
  typeName = let n = show . natVal $ (Proxy :: Proxy n)
             in const $ "int" ++ n
  isDynamic = const False

instance KnownNat n => ABIDecode (IntN n) where
  fromDataParser =
    let nBytes = natVal (Proxy :: Proxy n)
    in do
      a <- int256HexParser
      case intNFromInteger a of
        Nothing -> fail $ "Could not decode as " ++ typeName (Proxy :: Proxy (UIntN n)) ++ ": " ++ show a
        Just a' -> return a'
