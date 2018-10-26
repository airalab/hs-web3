Getting started
===============

Installation
~~~~~~~~~~~~

Using `Stackage <https://docs.haskellstack.org>`_

    stack install web3

Quick start
~~~~~~~~~~~

Lets import library entrypoint modules using `ghci`:

.. code-block:: haskell

    > import Network.Ethereum.Web3
    > import qualified Network.Ethereum.Api.Web3 as Web3

.. note::

   We recomends to import `Network.Ethereun.Api.Web3` as **qualified**, because it has name similar to their prefix in JSON-RPC API.

Looks anything in `Web3` API:

.. code-block:: haskell

    > :t Web3.clientVersion
    Web3.clientVersion :: JsonRpc m => m Text

To run it use `Web3` provider monad:

.. code-block:: haskell

    > :t runWeb3
    runWeb3 :: MonadIO m => Web3 a -> m (Either Web3Error a)

    > runWeb3 Web3.clientVersion
    Right "Parity-Ethereum//v2.0.3-unstable/x86_64-linux-gnu/rustc1.29.0"

.. note::
   Function ``runWeb3`` use default provider at **http://localhost:8545**, for using custom providers try ``runWeb3'``.
