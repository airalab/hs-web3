{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Codec.Scale.TH
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- It contains template haskell SCALE helper functions.
--

module Codec.Scale.TH where


import           Control.Monad       (replicateM)
import           Language.Haskell.TH (DecsQ, Type (VarT), appT, conT, cxt,
                                      instanceD, newName, tupleT)

import           Codec.Scale.Class   (Decode, Encode)

tupleInstances :: Int -> DecsQ
tupleInstances n = do
    vars <- replicateM n $ newName "a"
    let types = fmap (pure . VarT) vars
    sequence $
      [ instanceD (cxt $ map (appT $ conT ''Decode) types) (appT (conT ''Decode) (foldl appT (tupleT n) types)) []
      , instanceD (cxt $ map (appT $ conT ''Encode) types) (appT (conT ''Encode) (foldl appT (tupleT n) types)) []
      ]
