Ethereum accounts
=================

.. note::

   `Ethereum whitepaper <https://github.com/ethereum/wiki/wiki/White-Paper>`_ mention two types of accounts: smart contract and external owned account (EOA). But EOA only can send transaction to network. In this page EOA managing and generalized transaction sending is described.

**hs-web3** support a few kinds of EOA described in table below.

 ============== =======================================================================
  Type           Description
 ============== =======================================================================
  Default_       typically first of node accounts list, **should be unlocked**
  Personal_      available via ``personal_*`` JSON-RPC, **password required**
  PrivateKey_    derived from secp256k1 private key, use JSON-RPC `sendRawTransaction`
 ============== =======================================================================

.. _Default: http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Account-Default.html
.. _Personal: http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Account-Personal.html
.. _PrivateKey: http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Account-PrivateKey.html

All of them has an instance for `Account <http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Account-Class.html>`_ typeclass.

.. code-block:: haskell

   class MonadTrans t => Account a t | t -> a where
      -- | Run computation with given account credentials
      withAccount :: JsonRpc m => a -> t m b -> m b

      -- | Send transaction to contract, like a 'write' command
      send :: (JsonRpc m, Method args) => args -> t m TxReceipt

      -- | Call constant method of contract, like a 'read' command
      call :: (JsonRpc m, Method args, AbiGet result) => args -> t m result

The ``Account`` is a `multi-parameter typeclass <https://wiki.haskell.org/Multi-parameter_type_class>`_ that define most important EOA actions.

Account managing
~~~~~~~~~~~~~~~~

The first parameter of ``Account`` typeclass is an account internal params presended as independent data type.

.. code-block:: haskell 

   -- | Unlockable node managed account params
   data Personal = Personal 
      { personalAddress    :: !Address
      , personalPassphrase :: !Passphrase
      } deriving (Eq, Show)

In this example ``Personal`` data contains of two params: personal account address and password. For using account credentials ``Account`` typeclass provide special function ``withAccount``.

.. code-block:: haskell

   -- | Run computation with given account credentials
   withAccount :: JsonRpc m => a -> t m b -> m b

``withAccount`` function takes two arguments: account initialization parameters (some credentials like a password or private key) and computation to run it in given account context. Finally it returns ``JsonRpc`` computation that can be runned using any web3 provider.

.. code-block:: haskell

   runWeb3 $ do

      -- Run with default account context
      withAccount () $ ...

      -- Run with personal account context
      withAccount (Personal "0x..." "password") $ ...


Transaction sending
~~~~~~~~~~~~~~~~~~~


The second parameter of ``Account`` typeclass is transaction parametrization monad. This monad do one thing - prepare transaction parameters before call.

.. note::

   Transaction sending diagram by layer looks like ``provider -> account -> transaction``, provider at low level, account at middle layer and transaction former at high level.

``withParam`` is a special function, it behaviour is very similar to ``withStateT`` function. It used to set parameters of transaction locally and revert params after out of scope.

.. code-block:: haskell

   withParam :: Account p (AccountT p)
             => (CallParam p -> CallParam p)
             -> AccountT p m a
             -> AccountT p m a

The first argument of ``withParam`` function is state transition function, second - the computation to run in context of changed state. ``CallParam`` helps to parametrize transaction sending, `lenses <http://hackage.haskell.org/package/lens>`_ is very useful for this purpose.

.. code-block:: haskell

   runWeb3 $
      withAccount () $
         withParam (to .~ alice) $
            ...

Where lens ``to`` is used for setting transaction recipient address. All transaction parametrization lenses presended in table below.

 ============================================================================================================ ======================
  Lens                                                                                                        Description
 ============================================================================================================ ======================
  `to <http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Account.html#v:to>`_             Recipient address
  `value <http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Account.html#v:value>`_       Transaction value
  `gasLimit <http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Account.html#v:gasLimit>`_ Execution gas limit
  `gasPrice <http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Account.html#v:gasPrice>`_ Gas price
  `block <http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Account.html#v:block>`_       Execution block (for call only)
  `account <http://hackage.haskell.org/package/web3-0.8.1.0/docs/Network-Ethereum-Account.html#v:account>`_   Account credentials
 ============================================================================================================ ======================

.. note::

   By default transaction gas limit estimated according to transaction input but it also can be set manually.

Finally for sending transactions ``Account`` typeclass provide two functions:

.. code-block:: haskell

   -- | Send transaction to contract, like a 'write' command
   send :: (JsonRpc m, Method args) => args -> t m TxReceipt

   -- | Call constant method of contract, like a 'read' command
   call :: (JsonRpc m, Method args, AbiGet result) => args -> t m result

.. note::

   Functions above can be run in account context only and transaction parameters should be set before.

Safe transactions
~~~~~~~~~~~~~~~~~

Default behaviour of ``send`` function is send transaction and waiting for transaction receipt. It does mean that transaction is already in blockchain when execution flow get back. But finalization in Ethereum is probabilistic. For this reason waiting for some count of confirmation is a good practics for safe transaction sending.

.. note::

   Vitalik Buterin `blog post <https://blog.ethereum.org/2015/09/14/on-slow-and-fast-block-times/>`_ describe how much confirmation is required for high probability of transaction finality. For using this value import ``safeConfirmations`` from ``Network.Ethereum.Account.Safe`` module.

Module ``Network.Ethereum.Account.Safe`` implements function ``safeSend``. It very similar to ``send`` but take count of transaction confirmation as first argument.

.. code-block:: haskell

   send = safeSend 0

