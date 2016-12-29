## Ethereum Haskell API

This is the Ethereum compatible Haskell API which implements the [Generic JSON RPC](https://github.com/ethereum/wiki/wiki/JSON-RPC) spec.

[![Build Status](https://travis-ci.org/airalab/hs-web3.svg?branch=master)](https://travis-ci.org/airalab/hs-web3)
[![Build status](https://ci.appveyor.com/api/projects/status/8ljq93nar8kobk75?svg=true)](https://ci.appveyor.com/project/akru/hs-web3)
[![Hackage](https://img.shields.io/hackage/v/web3.svg)](http://hackage.haskell.org/package/web3)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/web3.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)

### Installation

    $ git clone https://github.com/airalab/hs-web3 && cd hs-web3
    $ stack setup
    $ stack ghci

> This library runs only paired with [geth](https://github.com/ethereum/go-ethereum)
> or [parity](https://github.com/ethcore/parity) Ethereum node,
> please start node first before using the library.

### Web3 monad

Any Ethereum node communication wrapped with `Web3` monadic type.

    > :t web3_clientVersion
    web3_clientVersion :: Provider a => Web3 a Text

To run this computation used `runWeb3'` or `runWeb3` functions.

    > runWeb3 web3_clientVersion
    Right "Parity//v1.4.5-beta-a028d04-20161126/x86_64-linux-gnu/rustc1.13.0"

Function `runWeb3` use default `Web3` provider at `localhost:8545`.

    > :t runWeb3
    runWeb3
      :: MonadIO m => Web3 DefaultProvider b -> m (Either Web3Error b)

### TemplateHaskell generator

[Quasiquotation](https://wiki.haskell.org/Quasiquotation) is used to parse
contract ABI or load from JSON file. [TemplateHaskell](https://wiki.haskell.org/Template_Haskell) driven Haskell contract API generator can automatical create instances for `Event` and `Method`
typeclasses and function helpers.

    > :set -XQuasiQuotes
    > putStr [abiFrom|data/sample.json|]
    Contract:
            Events:
                    Action1(address,uint256)
                    Action2(string,uint256)
            Methods:
                    0x03de48b3 runA1()
                    0x90126c7a runA2(string,uint256)

See example of usage.

```haskell
import Data.ByteArray (Bytes)
import Data.Text (Text)

[abiFrom|data/sample.json|]

main :: IO ()
main = do
    tx <- runWeb3 (runA2 addr nopay "Hello!" 42)
    print tx
  where addr = "0x19EE7966474b31225F71Ef8e36A71378a58a20E1"
```
