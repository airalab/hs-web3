Getting started
===============

.. note::
   **hs-web3** is a Haskell library. Of course you should have some knowledge about Haskell and platform tools like a `cabal` or `stack`. If you have not - `Real World Haskell <http://book.realworldhaskell.org>`_ and `Learn You a Haskell for Great Good <http://learnyouahaskell.com>`_ is a good point to begin. 

Installation
~~~~~~~~~~~~

**Simplest way** is using `Stackage <https://docs.haskellstack.org>`_ with `Nix <https://nixos.org/nix>`_ integration.

.. code-block:: bash

    stack install web3 --nix

Dependencies for building from source without Nix:

- `zlib <https://packages.ubuntu.com/ru/trusty/zlib1g-dev>`_
- `secp256k1 <https://launchpad.net/ubuntu/+source/libsecp256k1>`_
- optional: `solidity <https://solidity.readthedocs.io/en/v0.4.21/installing-solidity.html#binary-packages>`_

Quick start
~~~~~~~~~~~

Lets import library entrypoint modules using ``ghci``:

.. code-block:: haskell

    > import Network.Ethereum.Web3
    > import qualified Network.Ethereum.Api.Eth as Eth

.. note::

   I recomend to import `Network.Ethereun.Api.Eth` as **qualified**, because it has name similar to their prefix in JSON-RPC API.

Looks anything in ``Eth`` API:

.. code-block:: haskell

    > :t Eth.blockNumber
    Eth.blockNumber :: JsonRpc m => m Quantity

To run it use ``runWeb3`` function:

.. code-block:: haskell

    > :t runWeb3
    runWeb3 :: MonadIO m => Web3 a -> m (Either Web3Error a)

    > runWeb3 Eth.blockNumber 
    Right 6601059

.. note::

   Function ``runWeb3`` run default provider at **http://localhost:8545**, for using custom providers try to use ``runWeb3'``.

