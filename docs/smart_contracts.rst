Smart contracts
===============

If Ethereum is a World Computer then smart contract is a program for that. **hs-web3** provide functions and abstractions to compile, deploy and interact with smart contracts.

.. note::

   Currently **Solidity** is de facto standard for Ethereum smart contract development. Please read `intro <https://solidity.readthedocs.io/en/latest/introduction-to-smart-contracts.html>`_ to get more knowledge about Solidity smart contracts.

Contract ABI
~~~~~~~~~~~~

One of the most important thing that Solidity introduce is a `contract Application Binary Interface <https://solidity.readthedocs.io/en/latest/abi-spec.html>`_. ABI is a standard for smart contract communication, both from outside the Ethereum and for contract-to-contract interaction. In hs-web3 `Quasiquotation <https://wiki.haskell.org/Quasiquotation>`_ is used to parse contract JSON ABI or load from file. `TemplateHaskell <https://wiki.haskell.org/Template_Haskell>`_ driven generator creates ABI encoding instances and contract method helpers automatically.

.. code-block:: haskell

   {-# LANGUAGE DataKinds             #-}
   {-# LANGUAGE DeriveGeneric         #-}
   {-# LANGUAGE FlexibleContexts      #-}
   {-# LANGUAGE FlexibleInstances     #-}
   {-# LANGUAGE MultiParamTypeClasses #-}
   {-# LANGUAGE OverloadedStrings     #-}
   {-# LANGUAGE QuasiQuotes           #-}
   module ERC20 where

   import           Network.Ethereum.Contract.TH

   [abiFrom|ERC20.json|]

Using Solidity contract ABI generator creates helper functions like a ``transfer`` and ``balanceOf``.

.. code-block:: haskell

   transfer :: (JsonRpc m, Account a t, Functor (t m)) => Address -> UIntN 256 -> t m TxReceipt

   balanceOf :: (JsonRpc m, Account a t, Functor (t m)) => Address -> t m (UIntN 256)
 
.. note::

   Use ``-ddump-splices`` to see generated code during compilation or in GHCi.

Helper functions wraps building and sending transaction with given argument and `function selector <https://solidity.readthedocs.io/en/latest/abi-spec.html#function-selector>`_. This behaviour is very similar to `web3.js contract object <https://web3js.readthedocs.io/en/1.0/web3-eth-contract.html>`_.

ABI encoding
~~~~~~~~~~~~

To build transaction input from Solidity method call special encoding is used. **hs-web3** implements Solidity ABI encoding for `primitive <https://solidity.readthedocs.io/en/latest/abi-spec.html#types>`_ and composed types. Codecs are placed at ``Data.Solidity.Abi.Codec``.

.. code-block:: haskell

   encode :: (AbiPut a, ByteArray ba) => a -> ba

   decode :: (ByteArrayAccess ba, AbiGet a) => ba -> Either String a

.. note::

   When I develop codecs I was inspired by `cereal <http://hackage.haskell.org/package/cereal>`_ library. As result ``AbiGet`` and ``AbiPut`` classes are analogue to cereal ``Serialize``.

Primitive solidity types are placed at ``Data.Solidity.Prim``, this module exports types like an ``Address`` or ``UIntN``.

.. code-block:: haskell

   > import Data.Solidity.Prim   
   > import Data.Solidity.Abi.Codec
   > encode (42 :: UIntN 128) :: HexString
   HexString "0x000000000000000000000000000000000000000000000000000000000000002a"
   > encode (42 :: IntN 256, "Hello" :: Text) :: HexString
   HexString "0x000000000000000000000000000000000000000000000000000000000000002a0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000000548656c6c6f000000000000000000000000000000000000000000000000000000"

Contract deployment
~~~~~~~~~~~~~~~~~~~

To deploy smart contract special function with name ``new`` is used. 

.. code-block:: haskell

   -- | Create new smart contract on blockchain
   new :: (Account p t, JsonRpc m, Method a, Monad (t m))
       => a
       -- ^ Contract constructor
       -> t m (Maybe Address)
       -- ^ Address of deployed contract when transaction success

This function use ``Method`` instance of contract constructor (``*Contract`` data type) to encode transaction input and send it without destination to create new contract.

.. code-block:: haskell

   Just address <- runWeb3 $ withAccount () $ withParam id $ new SimpleStorageContract

