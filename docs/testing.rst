Testing
=======

Testing the **web3** is split up into two suites: **unit** and **live**.

- The unit suite tests internal library facilities.
- The live tests that the library adequately interacts with a Web3 provider.

One may simply run ``stack test`` to run both suites, or ``stack test web3:unit`` or ``stack test web3:live`` to run the test suites individually.

.. note::
   The live suite also requires a Web3 provider with Ethereum capabilities, as well as an unlocked account with ether to send transactions from.
