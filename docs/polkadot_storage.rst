Polkadot Storage
================

Blockchains that are built with Substrate expose a remote procedure call (RPC) server
that can be used to query runtime storage. In Haskell Web3 the standard Web3 provider could be used.

Lets try to query Polkadot storage with ``runWeb3'`` function using ``ghci``.

.. code-block:: haskell

   > import Network.Web3.Provider
   > import Network.Polkadot
   > runWeb3' (WsProvider "127.0.0.1" 9944) (query "timestamp" "now" [] :: Web3 (Either String Moment))
   Right (Right 1610689972001)

The ``query`` function arguments is **section** (or module), **method** and list of arguments (for maps and double maps).

.. code-block:: haskell

   query :: (JsonRpc m, Decode a) => Text -> Text -> [Argument] -> m a

Where ``a`` type should be SCALE decodable.

.. note::

    More usage details available in `Polkadot example <https://github.com/airalab/hs-web3/tree/master/examples/polkadot>`_  app. 
