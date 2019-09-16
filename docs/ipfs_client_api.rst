Ipfs Client API
=================

As many Ethereum Dapps use Ipfs for data storage, an `Ipfs Client Api <https://docs.ipfs.io/reference/api/http/>`_ has been included. 

.. note::

   The api client is placed at ``Network.Ipfs.Api``. ``Network.Ipfs.Api.Ipfs`` exports the api functions.

Api Type
~~~~~~~~

The api type is defined in ``Network.Ipfs.Api.Api``. The client uses the `Servant <https://www.servant.dev>`_ Library.

.. code-block:: haskell

    ipfsApi :: Proxy IpfsApi
    ipfsApi =  Proxy
 
    _cat :<|> _ls ... :<|> _shutdown = client ipfsApi

The aeson definitions of the data types returned by the api functions are also present in ``Network.Ipfs.Api.Api``. 

Monad Runner
~~~~~~~~~~~~

``Network.Ipfs.Api.Ipfs`` exports ``runIpfs`` monad runner and api functions.

.. code-block:: haskell

    runIpfs' :: BaseUrl -> Ipfs a -> IO ()

    runIpfs :: Ipfs a -> IO ()
    runIpfs = runIpfs' (BaseUrl Http "localhost" 5001 "/api/v0")

.. note::

   As you can see ``runIpfs`` uses the default BaseUrl at **http://localhost:5001/api/v0/** .

You can create a `BaseUrl <http://hackage.haskell.org/package/servant-client-core-0.16/docs/Servant-Client-Core-BaseUrl.html#t:BaseUrl>`_ instance for any other IPFS Api provider and pass it to **runIpfs'**.

Example of calling functions (Non-Stream) with runIpfs :

.. code-block:: haskell

    main = I.runIpfs $ do  
            ret <- I.cat <Cid>

Cid should be of the type Text.

Call Functions 
~~~~~~~~~~~~~~

There are three type of call functions:

 ================ =======================================================================
  Type             Description
 ================ =======================================================================
  call             Regular Call function.
  multipartCall    Call function for ‘multipart/form-data’. 
  streamCall       Call function for Streams. 
 ================ =======================================================================

As streamCall returns IO(), the API functions using it has to be called with **liftIO** (Specified in Function Documentation).

.. code-block:: haskell

    main = I.runIpfs $ do  
            ret3 <- liftIO $ I.repoVerify
