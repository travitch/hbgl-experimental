{-# LANGUAGE TypeFamilies #-}
module Data.Graph.ImmutableDigraph (
  -- PackedImmutableDigraph -- vector based
  DenseImmutableDigraph,
  SparseImmutableDigraph
  ) where

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.List ( foldl' )
import Data.Maybe ( fromMaybe )
import Data.Monoid

import Data.Graph.Interface
import Data.Graph.Marker.Dense
import Data.Graph.Marker.Sparse

type DenseImmutableDigraph = ImmutableDigraph DenseMarker
type SparseImmutableDigraph = ImmutableDigraph SparseMarker

data ImmutableDigraph (k :: * -> *) a b =
  Gr { graphRepr :: IntMap (Context (ImmutableDigraph k a b)) }

instance (Eq a, Eq b, Ord a, Ord b) => Eq (ImmutableDigraph k a b) where
  (Gr g1) == (Gr g2) = g1 == g2

instance (MarksVertices k) => Graph (ImmutableDigraph k n e) where
  type VertexLabel (ImmutableDigraph k n e) = n
  type EdgeLabel (ImmutableDigraph k n e) = e
  type VertexMarker (ImmutableDigraph k n e) = k

  empty = Gr IM.empty
  isEmpty = IM.null . graphRepr
  mkGraph ns es = Gr g1
    where
      g0 = foldl' addNode mempty ns
      g1 = foldl' addEdge g0 es
      addNode acc (v, lbl) = IM.insert v (Context [] v lbl []) acc
      addEdge acc (Edge src tgt lbl) = fromMaybe acc $ do
        Context sps sv svlbl sss <- IM.lookup src acc
        let c' = Context sps sv svlbl ((tgt, lbl) : sss)
            acc' = IM.insert src c' acc
        Context dps dv dvlbl dss <- IM.lookup tgt acc'
        let c'' = Context ((src, lbl) : dps) dv dvlbl dss
            acc'' = IM.insert tgt c'' acc'
        return acc''

instance (MarksVertices k) => InspectableGraph (ImmutableDigraph k n e) where
  context (Gr g) v = IM.lookup v g

instance (MarksVertices k) => VertexListGraph (ImmutableDigraph k n e) where
  labeledVertices = IM.foldrWithKey' toLabV [] . graphRepr
    where
      toLabV vid (Context _ _ l _) acc = (vid, l) : acc
  vertices = IM.keys . graphRepr
  numVertices = IM.size . graphRepr

instance (MarksVertices k) => IncidenceGraph (ImmutableDigraph k n e) where
  out (Gr g) v = fromMaybe [] $ do
    Context _ _ _ s <- IM.lookup v g
    return $ map (\(tgt, elbl) -> Edge v tgt elbl) s

instance (MarksVertices k) => BidirectionalGraph (ImmutableDigraph k n e) where
  inn (Gr g) v = fromMaybe [] $ do
    Context p _ _ _ <- IM.lookup v g
    return $ map (\(src, elbl) -> Edge src v elbl) p

instance (MarksVertices k) => AdjacencyGraph (ImmutableDigraph k n e) where
  foldSuc f seed (Gr g) v = fromMaybe seed $ do
    Context _ _ _ s <- IM.lookup v g
    return $ foldl' (\acc (tgt, el) -> f tgt el acc) seed s

instance (MarksVertices k) => BidirectionalAdjacencyGraph (ImmutableDigraph k n e) where
  foldPre f seed (Gr g) v = fromMaybe seed $ do
    Context p _ _ _ <- IM.lookup v g
    return $ foldl' (\acc (tgt, el) -> f tgt el acc) seed p

instance (MarksVertices k) => EdgeListGraph (ImmutableDigraph k n e) where
  edges = IM.foldrWithKey' toEdges [] . graphRepr
    where
      toEdges src (Context _ _ _ s) acc =
        foldl' (toEdge src) acc s
      toEdge src acc (dst, lbl) = Edge src dst lbl : acc
