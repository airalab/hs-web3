module Network.Ethereum.Web3.Contract (
    EventAction(..)
  , Method(..)
  , Event(..)
  ) where

import Control.Concurrent (ThreadId, threadDelay, forkIO)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Maybe (catMaybes)

import Network.Ethereum.Web3.Encoding
import Network.Ethereum.Web3.Address
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Api

data EventAction = ContinueEvent | TerminateEvent
  deriving (Show, Eq)

class ABIEncoding a => Event a where
    eventFilter :: a -> Address -> Filter

    event :: Address -> (a -> IO EventAction) -> Web3 ThreadId
    event = _event

_event :: Event a => Address -> (a -> IO EventAction) -> Web3 ThreadId
_event a f = do
    fid <- let ftyp = snd $ let x = undefined :: Event a => a
                            in  (f x, x)
           in  eth_newFilter (eventFilter ftyp a)
    cfg <- ask
    liftIO $ forkIO $
        let loop = do threadDelay 1000000
                      res <- runWeb3' cfg (eth_getFilterChanges fid)
                      case res of
                          Left e -> print e
                          Right [] -> loop
                          Right changes -> do
                              acts <- mapM f $
                                  catMaybes $ fmap parseChange changes
                              if any (== TerminateEvent) acts
                              then return ()
                              else loop
        in do loop
              runWeb3' cfg (eth_uninstallFilter fid)
              return ()
  where
    prepareTopics = fmap (T.drop 2) . drop 1
    parseChange c = fromData $
        T.append (T.concat (prepareTopics $ changeTopics c))
                 (T.drop 2 $ changeData c)

class ABIEncoding a => Method a where
    sendTransaction :: Address -> a -> Web3 TxHash
    call :: ABIEncoding b => Address -> CallMode -> a -> Web3 b
