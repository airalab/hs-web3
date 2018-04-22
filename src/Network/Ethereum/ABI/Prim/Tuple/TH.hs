{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Network.Ethereum.ABI.Prim.Tuple
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- This module is for internal usage only.
-- It contains tuple abi encoding template haskell generator.
--

module Network.Ethereum.ABI.Prim.Tuple.TH (tupleDecs) where


import           Control.Monad              (replicateM)
import           Language.Haskell.TH        (DecsQ, Type (VarT), appT, clause,
                                             conT, cxt, funD, instanceD,
                                             newName, normalB, tupleT)

import           Network.Ethereum.ABI.Class (ABIGet, ABIPut, ABIType (..))

tupleDecs :: Int -> DecsQ
tupleDecs n = do
    vars <- replicateM n $ newName "a"
    let types = fmap (pure . VarT) vars
    sequence $
      [ instanceD (cxt $ map (appT $ conT ''ABIType) types) (appT (conT ''ABIType) (foldl appT (tupleT n) types))
          [funD 'isDynamic [clause [] (normalB [|const False|]) []]]
      , instanceD (cxt $ map (appT $ conT ''ABIGet) types) (appT (conT ''ABIGet) (foldl appT (tupleT n) types)) []
      , instanceD (cxt $ map (appT $ conT ''ABIPut) types) (appT (conT ''ABIPut) (foldl appT (tupleT n) types)) [] ]
