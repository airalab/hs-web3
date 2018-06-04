module ContractConfig
  ( simpleStorageConfig
  , complexStorageConfig
  ) where

import Chanterelle.Internal.Types (ContractConfig, NoArgs, noArgs, constructorNoArgs)

--------------------------------------------------------------------------------
-- | SimpleStorage
--------------------------------------------------------------------------------

simpleStorageConfig
  :: ContractConfig NoArgs
simpleStorageConfig =
    { filepath : "./build/contracts/SimpleStorage.json"
    , name : "SimpleStorage"
    , constructor : constructorNoArgs
    , unvalidatedArgs : noArgs
    }

--------------------------------------------------------------------------------
-- | ComplexStorage
--------------------------------------------------------------------------------

complexStorageConfig
  :: ContractConfig NoArgs
complexStorageConfig =
  { filepath : "./build/contracts/ComplexStorage.json"
  , name : "ComplexStorage"
  , constructor : constructorNoArgs
  , unvalidatedArgs : noArgs
  }
