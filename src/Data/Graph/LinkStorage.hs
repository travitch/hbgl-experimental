{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.Graph.LinkStorage (
  LinkStorage(..),
  HashSetPair,
  SetPair,
  ListPair,
  LHMap,
  SHMap
  ) where

import Control.DeepSeq
import Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.Set as S
import qualified Data.HashMap.Strict as SHM
import qualified Data.HashMap.Lazy as LHM
import Data.Monoid

-- | n = node type, l = edge label type
class (Eq n, Eq l, Monoid (c n l)) => LinkStorage c n l where
  linkEmpty :: c n l
  linkNull :: c n l -> Bool
  linkInsert :: n -> l -> c n l -> c n l
  linkFold :: (n -> l -> acc -> acc) -> acc -> c n l -> acc
  -- | Remove all the links to the given node in this container
  -- (regardless of label)
  linkDeleteAll :: n -> c n l -> c n l
  -- | Remove all links to the given node with the given label
  linkDelete :: n -> l -> c n l -> c n l
  linkSize :: c n l -> Int

  -- | Convert a link storage data structure into a list of (Node,
  -- EdgeLabel) pairs.
  linkToList :: c n l -> [(n, l)]
  linkToList = linkFold tupleAcc []
    where
      tupleAcc n l acc = (n, l) : acc
  -- | Convert a list of (Node, EdgeLabel) pairs into LinkStorage
  linkFromList :: [(n, l)] -> c n l
  linkFromList = foldr (\(n,l) acc -> linkInsert n l acc) linkEmpty

-- | Adapt a hash set of pairs into a type of kind * -> * -> * for
-- compatibility with maps
newtype HashSetPair a b = HSP { unHSP :: HS.HashSet (a, b) }
instance (Hashable n, Hashable l, Eq n, Eq l) => LinkStorage HashSetPair n l where
  linkEmpty = HSP HS.empty
  linkNull = HS.null . unHSP
  linkInsert n l = HSP . HS.insert (n, l) . unHSP
  linkFold f seed = HS.foldr (\(n,l) acc -> f n l acc) seed . unHSP
  linkDeleteAll n = HSP . HS.filter ((/=n) . fst) . unHSP
  linkDelete n l = HSP . HS.filter (/=(n,l)) . unHSP
  linkSize = HS.size . unHSP
instance (Show n, Show l) => Show (HashSetPair n l) where
  show (HSP hs) = show hs
instance (Hashable n, Eq n, Hashable l, Eq l) => Eq (HashSetPair n l) where
  (HSP h1) == (HSP h2) = h1 == h2
instance (Hashable n, Eq n, Hashable l, Eq l) => Monoid (HashSetPair n l) where
  mempty = HSP HS.empty
  (HSP h1) `mappend` (HSP h2) =
    let h3 = h1 `mappend` h2
    in HSP (HS.size h3 `seq` h3)
instance (NFData n, NFData l) => NFData (HashSetPair n l) where
  rnf (HSP h) = h `deepseq` ()

newtype SetPair a b = SP { unSP :: S.Set (a, b) }
                    deriving (Eq)
instance (Ord n, Ord l, Eq n, Eq l) => LinkStorage SetPair n l where
  linkEmpty = SP S.empty
  linkNull = S.null . unSP
  linkInsert n l = SP . S.insert (n, l) . unSP
  linkFold f seed = S.fold (\(n, l) acc -> f n l acc) seed . unSP
  linkDeleteAll n = SP . S.filter ((/=n) . fst) . unSP
  linkDelete n l = SP . S.filter (/= (n,l)) . unSP
  linkSize = S.size . unSP
instance (Show n, Show l) => Show (SetPair n l) where
  show (SP s) = show s
instance (Ord n, Ord l) => Ord (SetPair n l) where
  compare (SP s1) (SP s2) = compare s1 s2
instance (Ord n, Ord l) => Monoid (SetPair n l) where
  mempty = SP S.empty
  (SP s1) `mappend` (SP s2) =
    let s3 = s1 `mappend` s2
    in SP (S.size s3 `seq` s3)
instance (NFData n, NFData l) => NFData (SetPair n l) where
  rnf (SP s) = s `deepseq` ()

newtype ListPair a b = LP { unLP :: [(a, b)] }
                     deriving (Eq)
instance (Eq n, Eq l) => LinkStorage ListPair n l where
  linkEmpty = LP []
  linkNull = null . unLP
  linkInsert n l = LP . ((n,l):) . unLP
  linkFold f seed = foldr (\(n,l) acc -> f n l acc) seed . unLP
  linkDeleteAll n = LP . filter ((/=n) . fst) . unLP
  linkDelete n l = LP . filter (/=(n,l)) . unLP
  linkSize = length . unLP
instance Monoid (ListPair n l) where
  mempty = LP []
  (LP l1) `mappend` (LP l2) =
    let l3 = l1 `mappend` l2
    in LP (length l3 `seq` l3)
instance (NFData n, NFData l) => NFData (ListPair n l) where
  rnf (LP l) = l `deepseq` ()

-- | oops fixme: these map-based implementations need to have
-- containers for link storage instead of simple 1-1 mappings
newtype SHMap a b = SHMap { unSHM :: SHM.HashMap a b }
                  deriving (Eq)
instance (Eq n, Eq l, Hashable n, Hashable l) => LinkStorage SHMap n l where
  linkEmpty = SHMap SHM.empty
  linkNull = SHM.null . unSHM
  linkInsert n l = SHMap . SHM.insert n l . unSHM
  linkFold f seed = SHM.foldrWithKey f seed . unSHM
  linkDeleteAll n = SHMap . SHM.filterWithKey (\k _ -> n /= k) . unSHM
  linkDelete n l = SHMap . SHM.filterWithKey (\k v -> n /= k || v /= l) . unSHM
  linkSize = SHM.size . unSHM
instance (Eq n, Eq l, Hashable n, Hashable l) => Monoid (SHMap n l) where
  mempty = SHMap SHM.empty
  (SHMap m1) `mappend` (SHMap m2) = SHMap (m1 `mappend` m2)
instance (NFData n, NFData l) => NFData (SHMap n l) where
  rnf (SHMap m) = m `deepseq` ()

newtype LHMap a b = LHMap { unLHM :: LHM.HashMap a b }
                  deriving (Eq)
instance (Eq n, Eq l, Hashable n, Hashable l) => LinkStorage LHMap n l where
  linkEmpty = LHMap LHM.empty
  linkNull = LHM.null . unLHM
  linkInsert n l = LHMap . LHM.insert n l . unLHM
  linkFold f seed = LHM.foldrWithKey f seed . unLHM
  linkDeleteAll n = LHMap . LHM.filterWithKey (\k _ -> n /= k) . unLHM
  linkDelete n l = LHMap . LHM.filterWithKey (\k v -> n /= k || v /= l) . unLHM
  linkSize = LHM.size . unLHM
instance (Eq n, Eq l, Hashable n, Hashable l) => Monoid (LHMap n l) where
  mempty = LHMap LHM.empty
  (LHMap m1) `mappend` (LHMap m2) = LHMap (m1 `mappend` m2)
instance (NFData n, NFData l) => NFData (LHMap n l) where
  rnf (LHMap m) = m `deepseq` ()