{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

-- |
-- Module      :  Network.Ethereum.Ens.PublicResolver
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum Name System public resolver smart contract.
--

module Network.Ethereum.Ens.PublicResolver where

import           Network.Ethereum.Contract.TH

[abi|[{"stateMutability":"view","inputs":[{"name":"interfaceID","type":"bytes4"}],"name":"supportsInterface","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"stateMutability":"view","inputs":[{"name":"node","type":"bytes32"},{"name":"contentTypes","type":"uint256"}],"name":"ABI","outputs":[{"name":"contentType","type":"uint256"},{"name":"data","type":"bytes"}],"payable":false,"type":"function"},{"stateMutability":"nonpayable","inputs":[{"name":"node","type":"bytes32"},{"name":"x","type":"bytes32"},{"name":"y","type":"bytes32"}],"name":"setPubkey","outputs":[],"payable":false,"type":"function"},{"stateMutability":"view","inputs":[{"name":"node","type":"bytes32"}],"name":"content","outputs":[{"name":"ret","type":"bytes32"}],"payable":false,"type":"function"},{"stateMutability":"view","inputs":[{"name":"node","type":"bytes32"}],"name":"addr","outputs":[{"name":"ret","type":"address"}],"payable":false,"type":"function"},{"stateMutability":"nonpayable","inputs":[{"name":"node","type":"bytes32"},{"name":"contentType","type":"uint256"},{"name":"data","type":"bytes"}],"name":"setABI","outputs":[],"payable":false,"type":"function"},{"stateMutability":"view","inputs":[{"name":"node","type":"bytes32"}],"name":"name","outputs":[{"name":"ret","type":"string"}],"payable":false,"type":"function"},{"stateMutability":"nonpayable","inputs":[{"name":"node","type":"bytes32"},{"name":"name","type":"string"}],"name":"setName","outputs":[],"payable":false,"type":"function"},{"stateMutability":"nonpayable","inputs":[{"name":"node","type":"bytes32"},{"name":"hash","type":"bytes32"}],"name":"setContent","outputs":[],"payable":false,"type":"function"},{"stateMutability":"view","inputs":[{"name":"node","type":"bytes32"}],"name":"pubkey","outputs":[{"name":"x","type":"bytes32"},{"name":"y","type":"bytes32"}],"payable":false,"type":"function"},{"stateMutability":"nonpayable","inputs":[{"name":"node","type":"bytes32"},{"name":"addr","type":"address"}],"name":"setAddr","outputs":[],"payable":false,"type":"function"},{"inputs":[{"name":"ensAddr","type":"address"}],"payable":false,"type":"constructor"},{"anonymous":false,"inputs":[{"indexed":true,"name":"node","type":"bytes32"},{"indexed":false,"name":"a","type":"address"}],"name":"AddrChanged","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"node","type":"bytes32"},{"indexed":false,"name":"hash","type":"bytes32"}],"name":"ContentChanged","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"node","type":"bytes32"},{"indexed":false,"name":"name","type":"string"}],"name":"NameChanged","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"node","type":"bytes32"},{"indexed":true,"name":"contentType","type":"uint256"}],"name":"ABIChanged","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"node","type":"bytes32"},{"indexed":false,"name":"x","type":"bytes32"},{"indexed":false,"name":"y","type":"bytes32"}],"name":"PubkeyChanged","type":"event"}]|]
