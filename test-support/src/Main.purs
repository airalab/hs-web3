module Main where

import Prelude

import Chanterelle (deployMain)
import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Types (DeployM, DeployConfig(..))
import ContractConfig (simpleStorageConfig, complexStorageConfig, linearizationConfig)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (Address, ETH, _from, _gas, defaultTransactionOptions)
import Network.Ethereum.Core.BigNumber (parseBigNumber, decimal)
import Node.FS.Aff (FS)
import Node.Process (PROCESS)
import Partial.Unsafe (unsafePartial)

main :: forall e. Eff (console :: CONSOLE, eth :: ETH, fs :: FS, process :: PROCESS, exception :: EXCEPTION | e) Unit
main = deployMain deployScript



type DeployResults =
  ( simpleStorage :: Address
  , complexStorage :: Address
  , linearization :: Address
  )

deployScript :: forall eff. DeployM eff (Record DeployResults)
deployScript = do
  deployCfg@(DeployConfig {primaryAccount}) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
  simpleStorage <- deployContract txOpts simpleStorageConfig
  complexStorage <- deployContract txOpts complexStorageConfig
  linearization <- deployContract txOpts linearizationConfig
  pure { simpleStorage: simpleStorage.deployAddress
       , complexStorage: complexStorage.deployAddress
       , linearization: linearization.deployAddress
       }
