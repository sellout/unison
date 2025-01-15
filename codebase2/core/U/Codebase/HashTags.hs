module U.Codebase.HashTags where

import Unison.Hash (Hash)
import Unison.Prelude

-- | Represents a hash of a type or term component
newtype ComponentHash = ComponentHash {unComponentHash :: Hash}
  deriving stock (Eq, Ord)

newtype BranchHash = BranchHash {unBranchHash :: Hash}
  deriving stock (Eq, Ord)

-- | Represents a hash of a causal containing values of the provided type.
newtype CausalHash = CausalHash {unCausalHash :: Hash}
  deriving stock (Eq, Ord)

newtype PatchHash = PatchHash {unPatchHash :: Hash}
  deriving stock (Eq, Ord)

instance Show ComponentHash where
  show h = "ComponentHash (" ++ show (unComponentHash h) ++ ")"

instance Show BranchHash where
  show h = "BranchHash (" ++ show (unBranchHash h) ++ ")"

instance Show CausalHash where
  show h = "CausalHash (" ++ show (unCausalHash h) ++ ")"

instance Show PatchHash where
  show h = "PatchHash (" ++ show (unPatchHash h) ++ ")"

instance From BranchHash Text where
  from = from @Hash @Text . unBranchHash

instance From CausalHash Text where
  from = from @Hash @Text . unCausalHash

instance From PatchHash Text where
  from = from @Hash @Text . unPatchHash
