{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Network.Ethereum.Web3.JsonAbi
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Ethereum smart contract JSON ABI parser.
--
module Network.Ethereum.Web3.JsonAbi where

import qualified Data.Text as T
import Data.Char (toLower)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Aeson.TH

data FunctionArg = FunctionArgs
  { funArgName :: Text
  , funArgType :: Text
  } deriving Show

$(deriveJSON (defaultOptions { fieldLabelModifier = map toLower . drop 6 }) ''FunctionArg)

data EventArg = EventArg
  { eveArgName    :: Text
  , eveArgType    :: Text
  , eveArgIndexed :: Bool
  } deriving Show

$(deriveJSON (defaultOptions { fieldLabelModifier = map toLower . drop 6 }) ''EventArg)

data Declaration
  = DConstructor { conInputs :: [FunctionArg] }
  | DFunction { funName      :: Text
              , funInputs    :: [FunctionArg]
              , funOutputs   :: Maybe [FunctionArg] }
  | DEvent { eveName      :: Text
           , eveInputs    :: [EventArg]
           , eveAnonymous :: Bool }
  | DFallback { falPayable :: Bool }
  deriving Show

$(deriveJSON (defaultOptions {
    sumEncoding = defaultTaggedObject { tagFieldName = "type" }
  , constructorTagModifier = map toLower
  , fieldLabelModifier = map toLower . drop 3 }) ''Declaration)

type ContractABI = [Declaration]

-- | Take a signature by given decl, e.g. foo(uint,string)
signature :: Declaration -> Text

signature (DConstructor inputs) = "(" <> args inputs <> ")"
  where args = T.dropEnd 1 . foldMap (<> ",") . fmap funArgType

signature (DFallback _) = "()"

signature (DFunction name inputs _) = name <> "(" <> args inputs <> ")"
  where args = T.dropEnd 1 . foldMap (<> ",") . fmap funArgType

signature (DEvent name inputs _) = name <> "(" <> args inputs <> ")"
  where args = T.dropEnd 1 . foldMap (<> ",") . inputTypes
        inputTypes = fmap eveArgType . filter eveArgIndexed
