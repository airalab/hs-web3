Ethereum Name Service
=====================

`ENS <https://ens.domains/>`_ offers a secure & decentralised way to address resources both on and off the blockchain using simple, human-readable names.

.. note::

   Experimental ENS on Ethereum mainnet added in release ``0.8``.

For resolving addresses from ENS names please use ``resolve`` function from `Network.Ethereum.Ens <http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Ens.html>`_.

.. code-block:: haskell

   import qualified Network.Ethereum.Ens as Ens
   import           Network.Ethereum.Web3
   import           Lens.Micro ((.~))

   main = runWeb3 $ withAccount () $ do
      alice <- Ens.resolve "aliceaccount.eth"

      withParam (to .~ alice) $
         withParam (value .~ (1 :: Ether)) $
            send ()

