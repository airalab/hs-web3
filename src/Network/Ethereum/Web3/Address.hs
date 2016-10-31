module Network.Ethereum.Web3.Address (
    Address
  , fromText
  , toText
  , zero
  )where

import Data.Text.Lazy.Builder.Int as B (hexadecimal)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Read as R (hexadecimal)
import Data.Text.Lazy (toStrict)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Control.Monad ((<=<))
import Prelude hiding (null)

newtype Address = Address { unAddress :: Integer }
  deriving (Eq, Ord)

instance Show Address where
    show = unpack . toText

fromText :: Text -> Either String Address
fromText = fmap (Address . fst) . R.hexadecimal <=< check
  where check t | T.take 2 t == "0x" = check (T.drop 2 t)
                | otherwise = if T.length t == 40 && T.all (flip elem valid) t
                              then Right t
                              else Left "This is not seems like address."
        valid = ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

toText :: Address -> Text
toText = toStrict . toLazyText . B.hexadecimal . unAddress

zero :: Address
zero = Address 0
