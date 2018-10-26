Smart contracts
===============

`Quasiquotation <https://wiki.haskell.org/Quasiquotation>`_ is used to parse contract ABI or load from JSON file. `TemplateHaskell <https://wiki.haskell.org/Template_Haskell>`_ driven Haskell contract API generator can automatical create ABI encoding instances and contract method helpers.

.. code-block:: haskell

    > :set -XQuasiQuotes
    > import Network.Ethereum.Contract.TH
    > putStr [abiFrom|data/sample.json|]
    Contract:
            Events:
                    Action1(address,uint256)
                    Action2(string,uint256)
            Methods:
                    0x03de48b3 runA1()
                    0x90126c7a runA2(string,uint256)

.. note::

   Use ``-ddump-splices`` to see generated code during compilation or in GHCi. See `examples <https://github.com/airalab/hs-web3/tree/master/examples>`_ for more use cases.
