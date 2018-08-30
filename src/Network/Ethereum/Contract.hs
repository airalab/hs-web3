{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- |
-- Module      :  Network.Ethereum.Contract
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
--
--

module Network.Ethereum.Contract where

import           Control.Monad.State              (StateT, get, runStateT,
                                                   withStateT)
import           Control.Monad.Trans              (lift)
import qualified Data.ByteArray                   as BA (convert)
import           Data.Default                     (Default (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Solidity.Abi.Codec          (encode)
import           Data.Solidity.Prim               (Address)
import           Lens.Micro                       ((.~), (^.))
import           Lens.Micro.TH                    (makeLenses)
import           Network.Ethereum.Account         (Account (..))
import qualified Network.Ethereum.Api.Eth         as Eth (call, sendTransaction)
import           Network.Ethereum.Api.Types       (Call (..),
                                                   DefaultBlock (Latest))
import           Network.Ethereum.Contract.Method (selector)
import           Network.Ethereum.Unit            (Unit (toWei))

data CallState = CallState
    { _target   :: Address
    , _value    :: Integer
    , _gas      :: Maybe Integer
    , _gasPrice :: Maybe Integer
    , _block    :: DefaultBlock
    } deriving (Eq, Show)

instance Default CallState where
    def = CallState "0x0000000000000000000000000000000000000000" def def def Latest

makeLenses ''CallState

type ContractT = StateT CallState

instance Account () ContractT where
    withAccount _ = fmap fst . flip runStateT def

    send (args :: a) = do
        s <- get
        lift $ Eth.sendTransaction $
            def { callTo = Just (s ^. target)
                , callGas = fmap fromInteger (s ^. gas)
                , callGasPrice = fmap fromInteger (s ^. gasPrice)
                , callValue = Just $ fromInteger (s ^. value)
                , callData = Just $ BA.convert dat }
      where dat = selector (Proxy :: Proxy a) <> encode args

    call (args :: a) = do
        s <- get
        lift $ flip Eth.call (s ^. block) $
            def { callTo = Just (s ^. target)
                , callData = Just $ BA.convert dat }
      where dat = selector (Proxy :: Proxy a) <> encode args

withTarget :: Address
           -> ContractT m a
           -> ContractT m a
{-# INLINE withTarget #-}
withTarget a = withStateT (target .~ a)

withValue :: Unit value
          => value
          -> ContractT m a
          -> ContractT m a
{-# INLINE withValue  #-}
withValue a = withStateT (value .~ toWei a)
