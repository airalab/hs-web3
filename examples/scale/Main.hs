{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Main where

import           Codec.Scale
import           Codec.Scale.Skip
import           Data.ByteArray.HexString (HexString)
import           Data.Word                (Word32)
import           Generics.SOP             (Generic)
import qualified GHC.Generics             as GHC (Generic)

data MyTransaction a = Tx
    { from    :: Word32
    , to      :: Word32
    , balance :: Compact Integer
    , note    :: Skip a
    }
    deriving (Show, Eq, GHC.Generic, Generic, Encode, Decode)

main :: IO ()
main = do
    let alice = 42
        bob = 15
        my_tx = Tx { from = alice
                   , to = bob
                   , balance = Compact 1000000
                   , note = Skip "Hello!"
                   }
    putStrLn "Encoded transaction:"
    print (encode my_tx :: HexString)
