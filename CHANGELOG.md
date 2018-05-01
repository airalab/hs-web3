# Changelog
All notable changes to this project will be documented in this file.

## [0.7.1.0] 2018-05-02
### Added
- TxReceipt and Eth.getTransactionReceipt JSON-RPC method.
- Keyword escaper for TH generated names.

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
