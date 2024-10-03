{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Network.Polkadot.Metadata.Type.Discovery
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Runtime type discovery for generic metadata structures.
--

module Network.Polkadot.Metadata.Type.Discovery
  ( DiscoveryContext
  , Discovery(..)
  , runDiscovery
  , prefix
  , types
  ) where

import           Control.Monad.State.Class      (MonadState (..))
import           Control.Monad.State.Strict     (runState)
import           Data.ByteArray.HexString       (HexString)
import           Data.Set                       (Set, insert)
import           Data.Text                      (Text)
import           Data.Word                      (Word8)
import           Generics.SOP
import           Lens.Micro.Extras              (view)
import           Lens.Micro.Mtl                 (use, (%=), (.=))
import           Lens.Micro.TH                  (makeLenses)

import           Network.Polkadot.Metadata.Type (Type (..))

-- | Contains information about types and current context.
data DiscoveryContext = DiscoveryContext
  { _prefix :: !Text
  , _types  :: !(Set Type)
  } deriving (Eq, Show)

instance Semigroup DiscoveryContext where
    (DiscoveryContext p a) <> (DiscoveryContext _ b) = DiscoveryContext p (a <> b)

instance Monoid DiscoveryContext where
    mempty = DiscoveryContext mempty mempty

makeLenses ''DiscoveryContext

-- | Collects information about runtime types.
class Discovery a where
    -- | Discover metadata structure for type information.
    discovery :: MonadState DiscoveryContext m
              => a
              -- ^ Input data structure that contains type information.
              -> m a
              -- ^ Returns the same structure wrapped with registry state monad.

-- | Skip 'Word8' when found.
instance {-# OVERLAPPING #-} Discovery Word8 where
    discovery = return

-- | Skip 'HexString' when found.
instance {-# OVERLAPPING #-} Discovery HexString where
    discovery = return

-- | Skip 'Text' when found.
instance {-# OVERLAPPING #-} Discovery Text where
    discovery = return

-- | Register 'Type' when found.
instance {-# OVERLAPPING #-} Discovery Type where
    discovery t = update . maybe t Type . flip typeOverlap t =<< use prefix
      where
        update x = types %= insert x >> return x

-- | Type overlapping hacks
typeOverlap :: Text
            -- ^ Module name
            -> Type
            -- ^ Module type
            -> Maybe Text
            -- ^ New type name
typeOverlap "Society" (Type "Vote")            = Just "SocietyVote"
typeOverlap "Treasury" (Type "Proposal")       = Just "TreasuryProposal"
typeOverlap "Assets" (Type "Balance")          = Just "TAssetBalance"
typeOverlap "Assets" (Type "Compact<Balance>") = Just "Compact<TAssetBalance>"
typeOverlap "Assets" (Type "Approval")         = Just "AssetApproval"
typeOverlap "Assets" (Type "ApprovalKey")      = Just "AssetApprovalKey"
typeOverlap "Assets" (Type "DestroyWitness")   = Just "AssetDestroyWitness"
typeOverlap "Identity" (Type "Judgement")      = Just "IdentityJudgement"
typeOverlap "ElectionProviderMultiPhase" (Type "Phase") = Just "ElectionPhase"
typeOverlap a (Type "Judgement")               = Just (a <> "Judgement")
typeOverlap a (Type "EquivocationProof")       = Just (a <> "EquivocationProof")
typeOverlap _ _                                = Nothing


-- | If input type is generic structure, let's go deep using generics.
instance (Generic a, GDiscovery (NS (NP I) (Code a))) => Discovery a where
    discovery = fmap (to . SOP) . gdiscovery . unSOP . from

-- | Generic version of 'Discovery' type class.
class GDiscovery a where
    gdiscovery :: MonadState DiscoveryContext m => a -> m a

-- | Discovery all constructors of the sum.
instance ( GDiscovery (NP I xs)
         , GDiscovery (NS (NP I) xss)
         ) => GDiscovery (NS (NP I) (xs ': xss)) where
    gdiscovery (Z xs) = Z <$> gdiscovery xs
    gdiscovery (S xs) = S <$> gdiscovery xs

-- | Finish when constructors will end.
instance GDiscovery (NS (NP I) '[]) where
    gdiscovery = return

-- | Discovery all fileds of constructors.
instance (Discovery a, GDiscovery (NP I as)) => GDiscovery (NP I (a ': as)) where
    gdiscovery (I a :* as) = do
        a' <- discovery a
        (I a' :*) <$> gdiscovery as

-- | Finish when fileds will end.
instance GDiscovery (NP I '[]) where
    gdiscovery = return

-- | Discovery types and returns sanitized metadata and set of discovered types.
runDiscovery :: (Discovery a, Traversable t) => (a -> Text) -> t a -> (t a, Set Type)
runDiscovery p = fmap (view types)
               . flip runState mempty
               . mapM (\m -> prefix .= p m >> discovery m)
