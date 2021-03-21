{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Data.ByteArray.HexString.TH
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Hex string template haskell helpers.
--

module Data.ByteArray.HexString.TH where

import           Data.ByteArray.HexString.Internal (HexString)
import           Language.Haskell.TH.Quote         (QuasiQuoter (..), quoteFile)

hexFrom :: QuasiQuoter
hexFrom = quoteFile hex

hex :: QuasiQuoter
hex = QuasiQuoter
    { quoteExp = \s -> [|fromString s :: HexString|]
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }
