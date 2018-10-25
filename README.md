Ethereum API for Haskell
========================

The Haskell Ethereum API which implements the [Generic JSON RPC](https://github.com/ethereum/wiki/wiki/JSON-RPC).

[![Documentation Status](https://readthedocs.org/projects/hs-web3/badge/?version=latest)](https://hs-web3.readthedocs.io/en/latest/?badge=latest)
[![Build Status](https://travis-ci.org/airalab/hs-web3.svg?branch=master)](https://travis-ci.org/airalab/hs-web3)
[![Hackage](https://img.shields.io/hackage/v/web3.svg)](http://hackage.haskell.org/package/web3)
[![LTS-12](http://stackage.org/package/web3/badge/lts-12)](http://stackage.org/lts-12/package/web3)
[![nightly](http://stackage.org/package/web3/badge/nightly)](http://stackage.org/nightly/package/web3)
[![Code Triagers](https://www.codetriage.com/airalab/hs-web3/badges/users.svg)](https://www.codetriage.com/airalab/hs-web3)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)

Install
-------

`stack install web3`

Usage
-----

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Basic imports
import           Network.Ethereum.Web3

-- Eth API support
import qualified Network.Ethereum.Api.Eth   as Eth
import           Network.Ethereum.Api.Types

-- ENS support
import qualified Network.Ethereum.Ens       as Ens

-- Lens to simple param setting
import           Lens.Micro                 ((.~))

main :: IO ()
main = do
    -- Use default provider on http://localhost:8545
    ret <- runWeb3 $ do

        -- Get address of default account
        me <- head <$> Eth.accounts

        -- Get balance of default account on latest block
        myBalance <- Eth.getBalance me Latest

        -- Get half of balance
        let halfBalance = fromWei (myBalance / 2)

        -- Use default account
        withAccount () $ do
            -- Get Ethereum address via ENS
            alice <- Ens.resolve "alice.address.on.eth"
            bob   <- Ens.resolve "bob.address.on.eth"

            -- Send transaction with value
            withParam (value .~ halfBalance) $ do

                -- Send transaction to alice account
                withParam (to .~ alice) $ send ()

                -- Send transaction to bob account
                withParam (to .~ bob) $ send ()

        -- Return sended value
        return halfBalance

    -- Web3 error handling
    case ret of
        Left e  -> error $ show e
        Right v -> print (v :: Ether)  -- Print returned value in ethers
```

---

Read more in the [documentation on ReadTheDocs](https://hs-web3.readthedocs.io).
