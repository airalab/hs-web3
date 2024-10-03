Web3 API for Haskell
====================

This library implements Haskell API client for popular Web3 platforms.

[![Documentation Status](https://readthedocs.org/projects/hs-web3/badge/?version=latest)](https://hs-web3.readthedocs.io/en/latest/?badge=latest)
[![CI](https://github.com/airalab/hs-web3/workflows/CI/badge.svg)](https://github.com/airalab/hs-web3/actions)
[![Hackage Matrix](https://matrix.hackage.haskell.org/api/v2/packages/web3/badge)](https://matrix.hackage.haskell.org/package/web3)
[![Hackage](https://img.shields.io/hackage/v/web3.svg)](http://hackage.haskell.org/package/web3)
[![LTS-22](http://stackage.org/package/web3/badge/lts-22)](http://stackage.org/lts-22/package/web3)
[![nightly](http://stackage.org/package/web3/badge/nightly)](http://stackage.org/nightly/package/web3)
[![Code Triagers](https://www.codetriage.com/airalab/hs-web3/badges/users.svg)](https://www.codetriage.com/airalab/hs-web3)

Install
-------

`stack install web3`

Usage
-----

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Basic imports
import           Network.Ethereum
import           Network.Web3

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

License
-------

* `Network.Polkadot` and `Codec.Scale` is licensed under [Apache 2.0](https://github.com/airalab/hs-web3/blob/master/LICENSE-APACHE2)
* All other source is licensed under [BSD-3-Clause](https://github.com/airalab/hs-web3/blob/master/LICENSE-BSD3)
