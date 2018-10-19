Ethereum API for Haskell
========================

The Haskell Ethereum API which implements the [Generic JSON RPC](https://github.com/ethereum/wiki/wiki/JSON-RPC).

[![Documentation Status](https://readthedocs.org/projects/hs-web3/badge/?version=latest)](https://hs-web3.readthedocs.io/en/latest/?badge=latest)
[![Build Status](https://travis-ci.org/airalab/hs-web3.svg?branch=master)](https://travis-ci.org/airalab/hs-web3)
[![Hackage](https://img.shields.io/hackage/v/web3.svg)](http://hackage.haskell.org/package/web3)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)
[![Code Triagers Badge](https://www.codetriage.com/airalab/hs-web3/badges/users.svg)](https://www.codetriage.com/airalab/hs-web3)

Installation
------------

Using [Stackage](https://docs.haskellstack.org):

    stack install web3

Quick start
-----------

Lets import library entrypoint modules using `ghci`:

    > import Network.Ethereum.Web3
    > import qualified Network.Ethereum.Api.Web3 as Web3

> We recomends to import `Network.Ethereun.Api.Web3` as **qualified**, because it has name similar to their prefix in JSON-RPC API.

Looks anything in `Web3` API:

    > :t Web3.clientVersion
    Web3.clientVersion :: JsonRpc m => m Text

To run it use `Web3` provider monad:

    > :t runWeb3
    runWeb3 :: MonadIO m => Web3 a -> m (Either Web3Error a)

    > runWeb3 Web3.clientVersion
    Right "Parity-Ethereum//v2.0.3-unstable/x86_64-linux-gnu/rustc1.29.0"

> Function `runWeb3` use default provider at `http://localhost:8545`, for using custom providers try `runweb3'`.

---

See [documentation](https://hs-web3.readthedocs.io) for other examples.

