Polkadot node API
=================

As same as Ethereum nodes Polkadot node exports HTTP/WebSockets `JSON-RPC` API. For connection with node **hs-web3** use internal tiny JSON-RPC client. 

Lets try to call Polkadot node with ``runWeb3'`` function using ``ghci``.

.. code-block:: haskell

   > import Network.Web3.Provider
   > import qualified Network.Polkadot.Api.System as System 
   > runWeb3' (WsProvider "127.0.0.1" 9944) $ System.name
   Right "Parity Polkadot"

It can be useful to define function with Polkadot node endpoint location.

.. code-block:: haskell

   myNode :: Web3 a -> Either Web3Error a
   myNode = runWeb3' (Wsprovider "127.0.0.1" 9944)

API Reference
~~~~~~~~~~~~~

Currently implemented the following Polkadot APIs in modules:

 ==================  ================
  Method prefix       Implementation
 ==================  ================
  ``account_*``       `Network.Polkadot.Api.Account <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-Account.html>`_
  ``author_*``        `Network.Polkadot.Api.Author <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-Author.html>`_
  ``babe_*``          `Network.Polkadot.Api.Babe <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-Babe.html>`_
  ``chain_*``         `Network.Polkadot.Api.Chain <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-Chain.html>`_
  ``childstate_*``    `Network.Polkadot.Api.Childstate <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-Childstate.html>`_
  ``contracts_*``     `Network.Polkadot.Api.Contracts <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-Contracts.html>`_
  ``engine_*``        `Network.Polkadot.Api.Engine <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-Engine.html>`_
  ``grandpa_*``       `Network.Polkadot.Api.Grandpa <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-Grandpa.html>`_
  ``offchain_*``      `Network.Polkadot.Api.Offchain <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-Offchain.html>`_
  ``payment_*``       `Network.Polkadot.Api.Payment <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-Payment.html>`_
  ``rpc_*``           `Network.Polkadot.Api.Rpc <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-Rpc.html>`_
  ``state_*``         `Network.Polkadot.Api.State <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-State.html>`_
  ``system_*``        `Network.Polkadot.Api.System <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-System.html>`_
 ==================  ================

All modules use descriptive types located at `Network.Polkadot.Api.Types <http://hackage.haskell.org/package/web3/docs/Network-Polkadot-Api-Types.html>`_.

.. note::
   
   See classic API reference at `Hackage web3 page <http://hackage.haskell.org/package/web3>`_.

