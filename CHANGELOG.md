# Changelog
All notable changes to this project will be documented in this file.

## [0.9.1.0] 2020-06-07
### Added
- SCALE codec implementation and tests 
- SCALE codec example 

### Changed
- Supported GHC version: >=8.4.4
- Imports optimized 

## [0.9.0.0] 2020-05-24
### Added
- Experimental IPFS REST API client
- Experimental Polkadot JSON-RPC API client

### Changed
- Web3 abstraction moved to `Network.Web3`
- Ethereum entry point renamed `Network.Ethereum.Web3` -> `Network.Ethereum`
- Split from common ECDSA staff from `Crypto.Ethereum` to `Crypto.Ecdsa`
- Ethereum related tests moved to `Network.Ethereum.Test`

### Removed
- Support for build on LTS-11, LTS-12, LTS-13

## [0.8.4.0] 2020-05-03
### Added
- Tuple support for `pragma experimental ABIEncoderV2`
- Fixed dependencies bounds for stackage LTS-15

### Changed
- Fixed reusing of HTTP request Manager

## [0.8.3.2] 2019-05-13
### Changed
- Fixed dependencies bounds for stackage distribution
- Optimized LANGUAGE pragmas

## [0.8.3.1] 2019-01-21
### Changed
- Fixed dependencies bounds for stackage distribution

## [0.8.3.0] 2019-01-09
### Added
- Web3 secret storage v3 support

### Changed 
- Cryptonite based ECC signer for messages and transactions, it removes secp256k1 dependency
- Fixed dependencies bounds for stackage LTS-13

## [0.8.2.1] 2018-11-19
### Changed
- Fixed dependencies bounds for stackage distribution

## [0.8.2.0] 2018-11-07
### Changed
- Gas estimation runs when gas limit is not set before

## [0.8.1.0] 2018-10-29
### Added
- Support vinyl-0.10 in `MultiFilter` module

## [0.8.0.0] 2018-10-26
### Added
- Support for Ethereum cryptography
- Local private key transaction signer 
- Generalized JSON-RPC monad for API methods
- Support for multiple transaction sending methods via one `Account` api
- Monad based transaction sending parametrization 
- Experimental support for solidity compiler (disabled by default) 
- Support for Ethereum mainnet ENS resolver
- Contract typeclass with api/bytecode getters
- Gas estimation for sending transactions
- Contract typeclass TH generator
- Function for creating contracts
- Event single/multi filters
- HexString data type
- Personal api calls
- Address checksum

### Changed
- package.yaml instead web3.cabal package descriptor
- Solidity related data types and codecs moved to Data.Solidity
- Solidity related parsers and compiler moved to Language.Solidity
- Modules in Network.Ethereum.Web3 moved to Network.Ethereum.Api
- fromWei/toWei from `Unit` typeclass now operates over `Integral`

### Removed
- `convert` function from `Unit` typeclass

## [0.7.3.0] 2018-05-22
### Added
- 'Network.Ethereum.ABI.Prim' meta-module as primitive types and instances aggregator.
- Stackage nightly build compatibility.

### Changed
- Potential nullable web3 type ('Change', 'Block', 'Transaction', etc.) fields are encoded as 'Maybe'.

## [0.7.2.0] 2018-05-13
### Added
- Generic JSON-RPC API documentation improvements.
- TH generator bug fixes.

### Changed
- Generic JSON-RPC API use fixed size byte arrays.

### Removed
- 'BlockNumber', 'FilterId' types replaced by 'Quantity' type

## [0.7.1.0] 2018-05-02
### Added
- TxReceipt and Eth.getTransactionReceipt JSON-RPC method.
- Keyword escaper for TH generated names.
- Build flag to enable TLS support.

### Changed
- Library dependencies bounds fixed for GHC 8.2.2-8.4.2 (stackage LTS + Nightly).

## [0.7.0.0] 2018-04-22
### Added
- This CHANGELOG.md file for significant changes tracking.
- Descriptive types for all JSON-RPC method parameters and returned values (#15).
- Widely use of basement:Word256 type for encoding.
- Full list of ethereum abi encoding types:
  * bool: `Bool`
  * int256: `IntN`
  * uint256: `UIntN`
  * string: `Text`
  * bytes: `Bytes`
  * bytes32: `BytesN`
  * dynamic array: `[]`
  * static array: `ListN`

### Changed
- Rewriten encoding engine for best performance, it now based on cereal:Serialize instead of parsec:Parser.
- Renamed encoding type classes and methods: `ABIEncode` -> `ABIPut`, `ABIDecode` -> `ABIGet`.
- Encoding related modules moved to Network.Ethereum.ABI.
- Primitive abi encoding types are moved to separated modules in Network.Ethereum.ABI.Prim.
- Contract interation related modules moved to Network.Ethereum.Contract.
- Ethereum node communication modules stay in Network.Ethereum.Web3.
- JSON-RPC tiny client is independent now and can be used separately.

### Removed
- `Event` type class, currently TH create `Data.Default` instance for `Filter e`.
- Custom setup for live testing (it replaced by travis script).
