module Network.Ethereum.Web3.Internal where

import Data.HexString (toText, fromBytes, toBytes, hexString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Char (toLower)
import Data.Text (Text)

hex :: Text -> Text
hex = toText . fromBytes . encodeUtf8

unhex :: Text -> Text
unhex = decodeUtf8 . toBytes . hexString . encodeUtf8

toLowerFirst :: String -> String
toLowerFirst [] = []
toLowerFirst (x : xs) = toLower x : xs

