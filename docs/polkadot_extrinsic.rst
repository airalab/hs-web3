Polkadot Extrinsic 
==================

Extrinsic is a piece of data from external world that proposed to be a part of blockchain. Generally exist two kinds of extrinsics: ``unsigned`` (inherents) and ``signed`` (transactions).

Lets try to send Polkadot transaction with ``runWeb3'`` function using ``ghci``.

.. code-block:: haskell

   > import Network.Web3.Provider
   > import Network.Polkadot

The first, let's create new one account.

.. code-block:: haskell

   > me <- generate :: IO Ed25519
   > multi_signer me
   "5D7c97BufUFEqrGGn2nyw5HhgMTzQT2YkBZ33mojWwBijFLQ"

Where ``Ed25519`` generated and its Base58 encoded address printed out. I've use ``multi_signer`` wrapper because of ``MultiAddress`` format used in Polkadot.

The next, let's make a call structure that encodes function arguments and parameters required for Polkadot runtime dispatcher.

.. code-block:: haskell

   > let Right alice = from_ss58check "5GrwvaEF5zXb26Fz9rcQpDWS57CtERHpNehXCPcNoHGKutQY"
   > Right myCall <- runWeb3' (WsProvider "127.0.0.1" 9944) $ new_call "Balances" "transfer" (MaId alice, Compact 200000000000000) 
   > myCall
   Call 0 5 (MaId 0xd43593c715fdd31c61141abd04a99fd6822c8558854ccde39a5684e7a56da27d, Compact 200000000000000)

Where ``alice`` is transfer destination account on chain, ``from_ss58check`` decodes it from Base58 and pack into ``AccountId`` type. It also should be wrapped in call arguments into ``MultiAddress`` type using ``MaId`` constructor. The ``new_call`` function gets module name, function name, arguments tuple and returns encodable structure for Polkadot runtime dispatcher.

Next step is signing the call and other extrinsic related staff like lifetime, nonce and etc. Fortunately, Haskell Web3 has ``sign_and_send`` function that makes it automatically.

.. code-block:: haskell

   > Right myTx <- runWeb3' (WsProvider "127.0.0.1" 9944) $ sign_and_send me myCall 0
   > myTx
   0x9034fb2c7e46b5de6e681565a657cefc32fb2aa93c21aad03acc20b79fb31e68

The ``sign_and_send`` function gets crypto pair to sign extrinsic, the call structure and tips amount (zero is acceptable in general case). If everything ok then you will get transaction hash as result.

.. note::

    More usage details available in `Polkadot example <https://github.com/airalab/hs-web3/tree/master/examples/polkadot>`_  app. 
