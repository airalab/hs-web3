## Ethereum Haskell API

This is the Ethereum compatible Haskell API which implements the [Generic JSON RPC](https://github.com/ethereum/wiki/wiki/JSON-RPC) spec.

[![Build Status](https://travis-ci.org/airalab/hs-web3.svg?branch=master)](https://travis-ci.org/airalab/hs-web3)
[![Build status](https://ci.appveyor.com/api/projects/status/8ljq93nar8kobk75?svg=true)](https://ci.appveyor.com/project/akru/hs-web3)
[![Hackage](https://img.shields.io/hackage/v/web3.svg)](http://hackage.haskell.org/package/web3)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)
[![Code Triagers Badge](https://www.codetriage.com/airalab/hs-web3/badges/users.svg)](https://www.codetriage.com/airalab/hs-web3)

### Installation

    $ git clone https://github.com/airalab/hs-web3 && cd hs-web3
    $ stack setup
    $ stack ghci

> This library runs only paired with [geth](https://github.com/ethereum/go-ethereum)
> or [parity](https://github.com/ethcore/parity) Ethereum node,
> please start node first before using the library.

### Web3 monad

Any Ethereum node communication wrapped with `Web3` monadic type.

    > import Network.Ethereum.Web3.Web3
    > :t clientVersion
    clientVersion :: Web3 Text

To run this computation used `runWeb3'` or `runWeb3` functions.

    > import Network.Ethereum.Web3
    > runWeb3 clientVersion
    Right "Parity//v1.4.5-beta-a028d04-20161126/x86_64-linux-gnu/rustc1.13.0"

Function `runWeb3` use default `Web3` provider at `localhost:8545`.

    > :t runWeb3
    runWeb3
      :: MonadIO m => Web3 a -> m (Either Web3Error a)

### TemplateHaskell generator

[Quasiquotation](https://wiki.haskell.org/Quasiquotation) is used to parse contract ABI or load from JSON file. [TemplateHaskell](https://wiki.haskell.org/Template_Haskell) driven Haskell contract API generator can automatical create ABI encoding instances and contract method helpers.

    > :set -XQuasiQuotes
    > import Network.Ethereum.Contract.TH
    > putStr [abiFrom|data/sample.json|]
    Contract:
            Events:
                    Action1(address,uint256)
                    Action2(string,uint256)
            Methods:
                    0x03de48b3 runA1()
                    0x90126c7a runA2(string,uint256)

Use `-ddump-splices` to see generated code during compilation or in GHCi. See `examples` folder for more use cases.

### Testing

Testing the `web3` is split up into two suites: `unit` and `live`.
The `unit` suite tests internal library facilities, while the `live` tests that
the library adequately interacts with a Web3 provider.

One may simply run `stack test` to run both suites, or `stack test web3:unit` or `stack test web3:live`
to run the test suites individually.

The `unit` suite has no external dependencies, while the `live` suite requires Truffle and `jq`
to be available on your machine.

The `live` suite also requires a Web3 provider with Ethereum capabilities, as well as
an unlocked account with ether to send transactions from. It uses Truffle to deploy testing contracts,
generating ABIs for them in the process, then using said ABIs as part of a TemplateHaskell step in the suite.
It is assumed that the provider is available at `http://localhost:8545`. If that's not the case, you must update `truffle.js`
so that Truffle can deploy the contracts correctly, and pass the `WEB3_PROVIDER=http://host:port` environment variable
when running the tests so that the `web3` library can interact with the chain that's being tested against.
