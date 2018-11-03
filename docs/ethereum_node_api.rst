Ethereum node API
=================

Any Ethereum node can export their `Generic JSON-RPC <https://github.com/ethereum/wiki/wiki/JSON-RPC>`_. For connection with node **hs-web3** use internal tiny JSON-RPC client. 

.. note::

   Tiny client library placed at ``Network.JsonRpc.TinyClient``. It exports special monad ``JsonRpc`` and function ``remote`` to define JSON-RPC methods. When developing tiny client I was inspired `HaXR library <http://hackage.haskell.org/package/haxr>`_.

Providers
~~~~~~~~~

To handle connection with Ethereum node some thing named **provider** is required. ``Provider`` data type define the endpoint of Ethereum node API. Module that export this type placed at ``Network.Ethereum.Api.Provider``. 

.. code-block:: haskell

   data Provider = HttpProvider String

.. note::

   Currently **hs-web3** support HTTP(S) providers only. 

Another interesting thing in this module is ``Web3`` type.

.. code-block:: haskell

   newtype Web3 a = ...
   instance Monad Web3
   instance JsonRpc Web3

As you can see ``Web3`` is monad that can handle JSON-RPC. It's very important because it can be used for any Ethereum node communication.

.. note::
   
   ``Web3`` is a `state monad <https://wiki.haskell.org/State_Monad>`_ with ``JsonRpcClient`` type as a state. This is mean that you can modify JSON-RPC server URI in runtime using `MTL lenses <http://hackage.haskell.org/package/microlens-mtl>`_ for example.

Finally provider module exports ``runWeb3`` and party functions.

.. code-block:: haskell

   runWeb3' :: MonadIO m => Provider -> Web3 a -> m (Either Web3Error a)

   runWeb3 :: MonadIO m => Web3 a -> m (Either Web3Error a)
   runWeb3 = runWeb3' def

.. note::

   Function ``runWeb3`` run default provider at **http://localhost:8545**.

Lets try to call custom Ethereum node URI with ``runWeb3'`` function using ``ghci``.

.. code-block:: haskell

   > import Network.Ethereum.Api.Provider
   > import qualified Network.Ethereum.Api.Eth as Eth
   > runWeb3' (HttpProvider "http://localhost:9545") Eth.blockNumber

It can be useful to define function with custom Ethereum node endpoint location.

.. code-block:: haskell

   myNode :: Web3 a -> Either Web3Error a
   myNode = runWeb3' (HttpProvider "http://my-host-name:8545")

API Reference
~~~~~~~~~~~~~

Currently implemented the following Ethereum APIs in modules:

 ===============  ================
  Method prefix    Implementation
 ===============  ================
  ``eth_*``        `Network.Ethereum.Api.Eth <http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Api-Eth.html>`_
  ``net_*``        `Network.Ethereum.Api.Net <http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Api-Net.html>`_
  ``web3_*``       `Network.Ethereum.Api.Web3 <http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Api-Web3.html>`_
  ``personal_*``   `Network.Ethereum.Api.Personal <http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Api-Personal.html>`_
 ===============  ================

All modules use descriptive types according to official Ethereum specification. It placed at `Network.Ethereum.Api.Types <http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Api-Types.html>`_.

.. note::
   
   See classic API reference at `Hackage web3 page <http://hackage.haskell.org/package/web3>`_.

