{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module Data.Graph.Interface (
  Graph(..),
  LNode(..),
  LEdge(..),
  Edge(..),
  Adj,
  Context(Context),
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
  ComparableGraph(..),
  -- * Graph traversals
  ufold,
  gmap,
  nmap,
  emap,
  -- * Context inspection
  lsuc',
  node',
  lab',
  lpre',
  suc',
  pre',
  out',
  inn',
  outdeg',
  indeg',
  labNode',
  neighbors'
  ) where

import qualified Data.Foldable as F

data LNode gr = LNode { unlabelNode :: Node gr,
                        nodeLabel :: NodeLabel gr
                      }
instance (Eq (Node gr), Eq (NodeLabel gr)) => Eq (LNode gr) where
  (LNode n1 l1) == (LNode n2 l2) = n1 == n2 && l1 == l2

data LEdge gr = LEdge { unlabelEdge :: Edge gr,
                        edgeLabel :: EdgeLabel gr
                      }
instance (Eq (Edge gr), Eq (EdgeLabel gr)) => Eq (LEdge gr) where
  (LEdge e1 l1) == (LEdge e2 l2) = e1 == e2 && l1 == l2

data Edge gr = Edge { edgeSource :: Node gr
                    , edgeDest :: Node gr
                    }
instance (Eq (Node gr)) => Eq (Edge gr) where
  (Edge s1 d1) == (Edge s2 d2) = s1 == s2 && d1 == d2
instance (Show (Node gr)) => Show (Edge gr) where
  show (Edge s d) = "Edge " ++ show s ++ " " ++ show d

type Adj gr = [(Node gr, EdgeLabel gr)]

-- | Inductive graph contexts
data Context gr = Context { contextIncomingLinks :: Adj gr
                          , contextNode :: LNode gr
                          , contextOutgoingLinks :: Adj gr
                          }

class Graph gr where
  type Node gr
  type NodeLabel gr
  type EdgeLabel gr
  mkGraph :: [LNode gr] -> [LEdge gr] -> gr
  isEmpty :: gr -> Bool

  empty :: gr
  empty = mkGraph [] []

class (Graph gr, Eq (Node gr)) => InspectableGraph gr where
  context :: gr -> Node gr -> Maybe (Context gr)
  -- | Test if a node is in the grap.  The default implementation can
  -- be overridden if desired
  gelem :: Node gr -> gr -> Bool
  gelem n g = maybe False (const True) (context g n)

  lab :: gr -> Node gr -> Maybe (NodeLabel gr)
  lab g n = do
    c <- context g n
    return (lab' c)

-- | An interface for graphs that can *efficiently* be decomposed into
-- a context and the remaining graph. FIXME: Define what should happen
-- to self loops in the matched context.  Should they be included or
-- excluded?  Including them has the advantage that match and & are
-- dual operations.
class (InspectableGraph gr) => DecomposableGraph gr where
  match :: Node gr -> gr -> Maybe (Context gr, gr)

-- | Graphs with efficient access to the outgoing edges for a given
-- node.  Minimum required implementation: 'lout'
class (Graph gr) => IncidenceGraph gr where
  lout :: gr -> Node gr -> [LEdge gr]

  out :: gr -> Node gr -> [Edge gr]
  out g = map unlabelEdge . lout g
  outDeg :: gr -> Node gr -> Int
  outDeg g = length . lout g

-- | Graphs with efficient access to incoming edges.  Minimum required
-- implementation: 'linn'
class (IncidenceGraph gr) => BidirectionalGraph gr where
  linn :: gr -> Node gr -> [LEdge gr]

  inn :: gr -> Node gr -> [Edge gr]
  inn g = map unlabelEdge . linn g
  inDeg :: gr -> Node gr -> Int
  inDeg g = length . linn g
  degree :: gr -> Node gr -> Int
  degree g n = outDeg g n + inDeg g n

-- | Graphs with efficient access to successor nodes.  Minimum
-- required implementation: 'lsuc' or 'suc'
class (InspectableGraph gr) => AdjacencyGraph gr where
  suc :: gr -> Node gr -> [Node gr]
  suc g = map unlabelNode . lsuc g

  -- | Suggested complexity: log(N)
  lsuc :: gr -> Node gr -> [LNode gr]
  lsuc g n = map (addLabel g) (suc g n)
    where
      addLabel gr s = case context gr s of
        Nothing -> error "lsuc: expected context not found"
        Just (Context _ ln _) -> ln

-- | Graphs with efficient access to predecessor nodes.  Minimum required
-- implementation: 'lpre' or 'pre'.
class (AdjacencyGraph gr) => BidirectionalAdjacencyGraph gr where
  pre :: gr -> Node gr -> [Node gr]
  pre g = map unlabelNode . lpre g

  lpre :: gr -> Node gr -> [LNode gr]
  lpre g n = map (addLabel g) (pre g n)
    where
      addLabel gr p = case context gr p of
        Nothing -> error "lpre: expected context not found"
        Just (Context _ ln _) -> ln
  -- FIXME: Remove duplicates
  lneighbors :: gr -> Node gr -> [LNode gr]
  lneighbors g n = lpre g n ++ lsuc g n
  neighbors :: gr -> Node gr -> [Node gr]
  neighbors g = map unlabelNode . lneighbors g

-- | Graphs with efficient access to all of the nodes in the graph.
-- Minimum required implementation: @labNodes@.
class (Graph gr) => VertexListGraph gr where
  -- | Suggested complexity: log(N)
  labNodes :: gr -> [LNode gr]

  nodes :: gr -> [Node gr]
  nodes = map unlabelNode . labNodes
  noNodes :: gr -> Int
  noNodes = length . labNodes

-- | Graphs with efficient access to all of the edges in the graph.
-- Minimum required implementation: @labEdges@.
class (Graph gr) => EdgeListGraph gr where
  -- | Suggested complexity: log(E)
  labEdges :: gr -> [LEdge gr]

  edges :: gr -> [Edge gr]
  edges = map unlabelEdge . labEdges
  noEdges :: gr -> Int
  noEdges = length . labEdges

-- | Graphs with efficient implementations of adjacency tests.
class (Graph gr) => AdjacencyMatrix gr where
  edgeExists :: gr -> Edge gr -> Maybe (EdgeLabel gr)

-- | This instance provides a default implementation for graphs that
-- can be inspected (defined in terms of 'context').
instance (InspectableGraph gr) => AdjacencyMatrix gr where
  edgeExists g e = do
    c <- context g src
    link <- F.find ((==dst) . fst) (F.toList (lsuc' c))
    return (snd link)
    where
      src = edgeSource e
      dst = edgeDest e

-- | The singular variants have default implementations.  They are
-- class members so that they can be overridden with more efficient
-- implementations if desired.  Minimal definition: '(&)'
--
-- FIXME: Provide a very precise definition for (&) and provide
-- default implementations of the graph modification functions based
-- on it.
class (DecomposableGraph gr, (Eq (EdgeLabel gr))) => MutableGraph gr where
  -- | This operator merges a context into the graph, producing a new
  -- graph.  The node whose context is being added must *not* exist in
  -- the graph.  If this invariant does not hold, the implementation
  -- should raise an error.  User code should probably eshew this
  -- function in favor of the more intuitive ins*, del*, and clear*
  -- functions.
  --
  -- Implementations must update pred and suc relations as necessary.
  (&) :: (Context gr) -> gr -> gr


  -- | Add the list of labeled edges to the graph.  The referenced
  -- nodes *MUST* exist in the graph.  If this condition is not met,
  -- behavior is undefined (recommended behavior: raise an error)
  insEdges :: [LEdge gr] -> gr -> gr
  insEdges es g = foldr insEdge g es

  -- | Add the list of labeled nodes to the graph.  Inserting an
  -- existing node with a different label should change the label in
  -- the graph.
  insNodes :: [LNode gr] -> gr -> gr
  insNodes ns g = foldr insNode g ns

  -- | Remove the nodes and all of their incoming and outgoing edges
  clearNodes :: [Node gr] -> gr -> gr
  clearNodes ns g = foldr clearNode g ns

  -- | Delete all of the given edges (deletes edges considering only
  -- endpoints - edge labels are not considered).
  delEdges :: [Edge gr] -> gr -> gr
  delEdges es g = foldr delEdge g es

  -- | Delete all of the given labeled edges
  delLEdges :: [LEdge gr] -> gr -> gr
  delLEdges es g = foldr delLEdge g es

  -- | Remove the nodes from the graph.  Raise an error if the node is
  -- connected to anything.
  delNodes :: [Node gr] -> gr -> gr
  delNodes ns g = foldr delNode g ns

  -- | Insert an edge into the graph.  Both the source and destination
  -- nodes of the edge must exist in the graph (otherwise an error
  -- will be raised).
  insEdge :: LEdge gr -> gr -> gr
  insEdge (LEdge (Edge src dst) lbl) g =
    case match src g of
      Nothing -> error "insEdge: source is not in graph"
      Just (Context p ln s, g') -> Context p ln ((dst, lbl) : s) & g'

  -- | Delete a node from the graph.  It is an error if the node
  -- does not exist in the graph.
  delNode :: Node gr -> gr -> gr
  delNode n g =
    case match n g of
      Nothing -> error "delNode: node does not exist in graph"
      Just (_, g') -> g'

  -- | Remove all of the incoming and outgoing edges from a node (but
  -- leave the node itself in the graph).  It is an error if the node
  -- does not exist in the graph.
  clearNode :: Node gr -> gr -> gr
  clearNode n g =
    case match n g of
      Nothing -> error "clearNode: node does not exist in graph"
      Just (Context _ ln _, g') -> (Context [] ln []) & g'

  -- | Delete all edges with the given endpoints (regardless of label)
  delEdge :: Edge gr -> gr -> gr
  delEdge (Edge src dst) g =
    case match src g of
      Nothing -> error "delEdge: source edge not in graph"
      Just (Context p ln s, g') ->
        Context p ln (filter ((/=dst) . fst) s) & g'

  -- | Delete a labeled edge from the graph
  delLEdge :: LEdge gr -> gr -> gr
  delLEdge (LEdge (Edge src dst) lbl) g =
    case match src g of
      Nothing -> error "delLEdge: source edge not in graph"
      Just (Context p ln s, g') -> Context p ln (filter (/=(dst,lbl)) s) & g'

  insNode :: LNode gr -> gr -> gr
  insNode ln g = (Context [] ln []) & g

-- FIXME: Instead of throwing errors, add overridable hooks that can
-- either throw an error or act as id?


-- | This class is meant for graphs that have efficient equality
-- tests.
class (Graph gr) => ComparableGraph gr where
  graphEqual :: gr -> gr -> Bool

-- | Fold a function over all of the contexts of the graph
ufold :: (VertexListGraph gr, InspectableGraph gr)
         => (Context gr -> c -> c) -- ^ Fold function
         -> c                    -- ^ Seed value
         -> gr                   -- ^ Graph
         -> c
ufold f seed g = foldr f seed ctxts
  where
    errMsg = "ufold: context lookup failure"
    ctxts = map (maybe (error errMsg) id) $ map (context g) (nodes g)

-- | Map a function over a graph.  The function can perform arbitrary
-- modifications to each context (which are combined to form a new
-- graph).
gmap :: (MutableGraph gr2, InspectableGraph gr1, VertexListGraph gr1)
        => (Context gr1 -> Context gr2)
        -> gr1
        -> gr2
gmap f = ufold (\c -> (f c&)) empty

-- | Map a function over the node labels of a graph
nmap :: (Node gr1 ~ Node gr2, EdgeLabel gr1 ~ EdgeLabel gr2,
         MutableGraph gr2, InspectableGraph gr1, VertexListGraph gr1)
        => (NodeLabel gr1 -> NodeLabel gr2)
        -> gr1
        -> gr2
nmap f = gmap (\(Context p (LNode v l) s) -> Context p (LNode v (f l)) s)


-- | Map a function over the edge labels of a graph
emap :: (Node gr1 ~ Node gr2, NodeLabel gr1 ~ NodeLabel gr2,
         MutableGraph gr2, InspectableGraph gr1, VertexListGraph gr1)
        => (EdgeLabel gr1 -> EdgeLabel gr2)
        -> gr1
        -> gr2
emap f = gmap (\(Context p (LNode v l) s) -> Context (adjMap f p) (LNode v l) (adjMap f s))
  where
    adjMap g = map (\(v, l) -> (v, g l))



-- Context inspection

lsuc' :: Context gr -> [(Node gr, EdgeLabel gr)]
lsuc' = contextOutgoingLinks

node' :: Context gr -> Node gr
node' (Context _ n _) = unlabelNode n

lab' :: Context gr -> NodeLabel gr
lab' (Context _ n _) = nodeLabel n

-- | This is a free function because the contextIncomingLinks field of
-- Contexts might not be useful if the graph is not bidirectional.
-- This free function has a context to enforce the constraint.
lpre' :: (BidirectionalGraph gr) => Context gr -> [(Node gr, EdgeLabel gr)]
lpre' = contextIncomingLinks

suc' :: (Graph gr) => Context gr -> [Node gr]
suc' = F.toList . fmap fst . lsuc'

pre' :: (BidirectionalGraph gr) => Context gr -> [Node gr]
pre' = map fst . lpre'

out' :: (Graph gr) => Context gr -> [LEdge gr]
out' (Context _ (LNode src _) s) = map (toEdge src) s
  where
    toEdge source (dst, lbl) = LEdge (Edge source dst) lbl

inn' :: (BidirectionalGraph gr) => Context gr -> [LEdge gr]
inn' (Context p (LNode dst _) _) = map (toEdge dst) p
  where
    toEdge dest (src, lbl) = LEdge (Edge src dest) lbl

outdeg' :: (Graph gr) => Context gr -> Int
outdeg' (Context _ _ s) = length s

indeg' :: (BidirectionalGraph gr) => Context gr -> Int
indeg' (Context p _ _) = length p

-- | Get the labeled node represented by the context
labNode' :: Context gr -> LNode gr
labNode' (Context _ (LNode n l) _) = LNode n l

-- | All nodes linked to or from in the context.  This function *may*
-- return duplicates if some node is both a predecessor and a
-- successor.
neighbors' :: (BidirectionalGraph gr) => Context gr -> [Node gr]
neighbors' (Context p _ s) = map fst (p ++ s)
