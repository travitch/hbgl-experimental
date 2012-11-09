{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances #-}
module Data.Graph.Interface (
  Vertex,
  Edge(..),
  Graph(..),
  Adj,
  Context(..),
  InspectableGraph(..),
  DecomposableGraph(..),
  IncidenceGraph(..),
  BidirectionalGraph(..),
  AdjacencyGraph(..),
  BidirectionalAdjacencyGraph(..),
  AdjacencyMatrix(..),
  VertexListGraph(..),
  EdgeListGraph(..),
  MutableGraph(..),
  MarksVertices(..),
  -- * Generic mutator
  (&),
  -- * Graph traversals
  ufold,
  gmap,
  nmap,
  emap,
  -- * Context inspection
  suc',
  pre',
  out',
  inn',
  outdeg',
  indeg',
  neighbors'
  ) where

import Control.Monad.ST
import qualified Data.Foldable as F
import Data.Hashable
import Data.Maybe ( fromMaybe, isJust )
import qualified Data.Set as S

type Vertex = Int

data Edge gr = Edge { edgeSource :: Vertex
                    , edgeTarget :: Vertex
                    , edgeLabel :: EdgeLabel gr
                    }

instance Show (EdgeLabel gr) => Show (Edge gr) where
  show (Edge s d lbl) = "Edge " ++ show s ++ " " ++ show d ++ " " ++ show lbl

instance (Eq (EdgeLabel gr)) => Eq (Edge gr) where
  (Edge s1 d1 l1) == (Edge s2 d2 l2) = s1 == s2 && d1 == d2 && l1 == l2

instance (Ord (EdgeLabel gr)) => Ord (Edge gr) where
  compare (Edge s1 d1 l1) (Edge s2 d2 l2) =
    compare (s1, d1, l1) (s2, d2, l2)

instance (Hashable (EdgeLabel gr)) => Hashable (Edge gr) where
  hash (Edge s1 d1 l1) = s1 `combine` d1 `combine` hash l1

type Adj gr = [(Vertex, EdgeLabel gr)]
data Context gr = Context { lpre' :: Adj gr
                          , vertex' :: Vertex
                          , lab' :: VertexLabel gr
                          , lsuc' :: Adj gr
                          }

instance (Eq (VertexLabel gr), Eq (EdgeLabel gr), Ord (VertexLabel gr), Ord (EdgeLabel gr)) => Eq (Context gr) where
  (Context ps1 n1 l1 ss1) == (Context ps2 n2 l2 ss2) =
    n1 == n2 && l1 == l2 && S.fromList ps1 == S.fromList ps2 &&
      S.fromList ss1 == S.fromList ss2

class MarksVertices (k :: * -> *) where
  newMarker :: (VertexListGraph gr) => gr -> ST s (k s)
  markVertex :: k s -> Vertex -> ST s ()
  isVertexMarked :: k s -> Vertex -> ST s Bool

class MarksVertices (VertexMarker gr) => Graph gr where
  type EdgeLabel gr
  type VertexLabel gr
  type VertexMarker gr :: * -> *

  isEmpty :: gr -> Bool
  empty :: gr
  mkGraph :: [(Vertex, VertexLabel gr)] -> [Edge gr] -> gr
  maxVertex :: gr -> Vertex

class (Graph gr) => InspectableGraph gr where
  context :: gr -> Vertex -> Maybe (Context gr)
  -- | Test if a node is in the grap.  The default implementation can
  -- be overridden if desired
  gelem :: Vertex -> gr -> Bool
  gelem n g = isJust (context g n)

  lab :: gr -> Vertex -> Maybe (VertexLabel gr)
  lab g n = do
    c <- context g n
    return (lab' c)

-- | An interface for graphs that can *efficiently* be decomposed into
-- a context and the remaining graph. FIXME: Define what should happen
-- to self loops in the matched context.  Should they be included or
-- excluded?  Including them has the advantage that match and & are
-- dual operations.
class (InspectableGraph gr) => DecomposableGraph gr where
  match :: Vertex -> gr -> Maybe (Context gr, gr)

-- | Graphs with efficient access to the outgoing edges for a given
-- node.  Minimum required implementation: 'out'
class (Graph gr) => IncidenceGraph gr where
  out :: gr -> Vertex -> [Edge gr]

  outDeg :: gr -> Vertex -> Int
  outDeg g = length . out g

-- | Graphs with efficient access to incoming edges.  Minimum required
-- implementation: 'inn'
class (IncidenceGraph gr) => BidirectionalGraph gr where
  inn :: gr -> Vertex -> [Edge gr]

  inDeg :: gr -> Vertex -> Int
  inDeg g = length . inn g
  degree :: gr -> Vertex -> Int
  degree g n = outDeg g n + inDeg g n

-- | Graphs with efficient access to successor nodes.  Minimum
-- required implementation: InspectableGraph
class (InspectableGraph gr) => AdjacencyGraph gr where
  foldSuc :: (Vertex -> EdgeLabel gr -> a -> a) -> a -> gr -> Vertex -> a

  suc :: gr -> Vertex -> [Vertex]
  suc g = map fst . lsuc g

  -- | Suggested complexity: log(N)
  lsuc :: gr -> Vertex -> [(Vertex, EdgeLabel gr)]
  lsuc g = maybe [] lsuc' . context g

-- | Graphs with efficient access to predecessor nodes.  Minimum required
-- implementation: 'lpre' or 'pre'.
class (AdjacencyGraph gr) => BidirectionalAdjacencyGraph gr where
  foldPre :: (Vertex -> EdgeLabel gr -> a -> a) -> a -> gr -> Vertex -> a

  pre :: gr -> Vertex -> [Vertex]
  pre g = map fst . lpre g

  lpre :: gr -> Vertex -> [(Vertex, EdgeLabel gr)]
  lpre g = maybe [] lpre' . context g
  neighbors :: gr -> Vertex -> [Vertex]
  neighbors g n = pre g n ++ suc g n

-- | Graphs with efficient access to all of the nodes in the graph.
-- Minimum required implementation: @labNodes@.
class (Graph gr) => VertexListGraph gr where
  -- | Suggested complexity: log(N)
  labeledVertices :: gr -> [(Vertex, VertexLabel gr)]

  vertices :: gr -> [Vertex]
  vertices = map fst . labeledVertices
  numVertices :: gr -> Int
  numVertices = length . vertices

-- | Graphs with efficient access to all of the edges in the graph.
-- Minimum required implementation: @edges@.
class (Graph gr) => EdgeListGraph gr where
  edges :: gr -> [Edge gr]
  numEdges :: gr -> Int
  numEdges = length . edges

-- | Graphs with efficient implementations of adjacency tests.
class (Graph gr) => AdjacencyMatrix gr where
  edgeExists :: gr -> Vertex -> Vertex -> [EdgeLabel gr]

-- | The singular variants have default implementations.  They are
-- class members so that they can be overridden with more efficient
-- implementations if desired.
class (Graph gr) => MutableGraph gr where
  -- | Insert an edge into the graph.  Edges will be inserted only if
  -- both of the endpoints are in the graph.  For graphs that do not
  -- allow multi-edges, inserting an edge that already exists will
  -- overwrite the edge label.  For multigraphs, a duplicate edge will
  -- be inserted.
  insertEdge :: Vertex -> Vertex -> EdgeLabel gr -> gr -> Maybe gr

  -- | Add a vertex to the graph.  If the vertex already exists
  -- in the graph, its label is overwritten.
  insertVertex :: Vertex -> VertexLabel gr -> gr -> gr

  -- | Delete a node from the graph.
  removeVertex :: Vertex -> gr -> gr

  -- | Remove all of the incoming and outgoing edges from a node (but
  -- leave the node itself in the graph).
  clearVertex :: Vertex -> gr -> gr

  -- | Remove the edge referenced by the given descriptor (obtained
  -- from insertEdge)
  removeEdge :: (Eq (EdgeLabel gr)) => Edge gr -> gr -> gr

  -- | Remove all edges with the given endpoints (regardless of label)
  removeEdges :: Vertex -> Vertex -> gr -> gr

-- FIXME Make this a package-private function.  It is kind of ugly.

-- | This function is the merge operator from FGL.  It has some
-- important invariants that /must/ be obeyed.  The vertex whose
-- context is being inserted must /not/ be in the graph.
-- Additionally, the other vertices referred to in the context /must/
-- exist in the graph.
(&) :: (MutableGraph gr) => Context gr -> gr -> gr
Context ps n l ss & g0 =
  let g1 = insertVertex n l g0
      addSucEdge (d, el) g = fromMaybe g (insertEdge n d el g)
      addPreEdge (s, el) g = fromMaybe g (insertEdge s n el g)
      g2 = foldr addSucEdge g1 ss
  in foldr addPreEdge g2 ps

-- | Fold a function over all of the contexts of the graph
ufold :: (VertexListGraph gr, InspectableGraph gr)
         => (Context gr -> c -> c) -- ^ Fold function
         -> c                    -- ^ Seed value
         -> gr                   -- ^ Graph
         -> c
ufold f seed g = foldr f seed ctxts
  where
    errMsg = "ufold: context lookup failure"
    errLookup = fromMaybe (error errMsg)
    ctxts = map (errLookup . context g) (vertices g)

-- | Map a function over a graph.  The function can perform arbitrary
-- modifications to each context (which are combined to form a new
-- graph).
gmap :: (MutableGraph gr2, InspectableGraph gr1, VertexListGraph gr1)
        => (Context gr1 -> Context gr2)
        -> gr1
        -> gr2
gmap f = ufold (\c -> (f c&)) empty

-- | Map a function over the node labels of a graph
nmap :: (EdgeLabel gr1 ~ EdgeLabel gr2,
         MutableGraph gr2, InspectableGraph gr1, VertexListGraph gr1)
        => (VertexLabel gr1 -> VertexLabel gr2)
        -> gr1
        -> gr2
nmap f = gmap (\(Context p v l s) -> Context p v (f l) s)


-- | Map a function over the edge labels of a graph
emap :: (VertexLabel gr1 ~ VertexLabel gr2,
         MutableGraph gr2, InspectableGraph gr1, VertexListGraph gr1)
        => (EdgeLabel gr1 -> EdgeLabel gr2)
        -> gr1
        -> gr2
emap f = gmap (\(Context p v l s) -> Context (adjMap f p) v l (adjMap f s))
  where
    adjMap g = map (\(v, l) -> (v, g l))

-- Context inspection

suc' :: (Graph gr) => Context gr -> [Vertex]
suc' = F.toList . fmap fst . lsuc'

pre' :: (BidirectionalGraph gr) => Context gr -> [Vertex]
pre' = map fst . lpre'

out' :: (Graph gr) => Context gr -> [Edge gr]
out' (Context _ src _ s) = map (toEdge src) s
  where
    toEdge source (dst, lbl) = Edge source dst lbl

inn' :: (BidirectionalGraph gr) => Context gr -> [Edge gr]
inn' (Context p dst _ _) = map (toEdge dst) p
  where
    toEdge dest (src, lbl) = Edge src dest lbl

outdeg' :: (Graph gr) => Context gr -> Int
outdeg' (Context _ _ _ s) = length s

indeg' :: (BidirectionalGraph gr) => Context gr -> Int
indeg' (Context p _ _ _) = length p

-- | All nodes linked to or from in the context.  This function *may*
-- return duplicates if some node is both a predecessor and a
-- successor.
neighbors' :: (BidirectionalGraph gr) => Context gr -> [Vertex]
neighbors' (Context p _ _ s) = map fst (p ++ s)
