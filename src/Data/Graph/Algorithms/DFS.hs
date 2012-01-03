{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | This module defines variants of depth-first search algorithms for
-- decomposable graphs.  An alternative module will define variants
-- that work for non-decomposable graphs (that variant will require
-- (Node gr) to be an instance of Ord).
module Data.Graph.Algorithms.DFS (
  -- * Directed DFS
  dfs,
  dfs',
  dff,
  dff',
  dfsWith,
  dfsWith',
  dffWith,
  dffWith',
  xdfsWith,
  xdfWith,
  xdffWith,
  -- * Undirected DFS
  udfs,
  udfs',
  udff,
  udff',
  -- * Reverse DFS
  rdff,
  rdff',
  rdfs,
  rdfs',
  -- * Applications
  topsort,
  topsort',
  scc,
  condense,
  reachable,
  components,
  noComponents,
  isConnected
  ) where

import Data.Map ( Map, (!) )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree
import Data.Graph.Interface
import Data.Graph.Algorithms.Basic

-- | Call the given function with all of the nodes in the graph as the
-- parameter
fixNodes :: (VertexListGraph gr)
            => ([Node gr] -> gr -> c)
            -> gr
            -> c
fixNodes f g = f (nodes g) g

-- | General depth-first search
xdfsWith :: (DecomposableGraph gr)
                  => (Context gr -> [Node gr])
                  -> (Context gr -> c)
                  -> [Node gr]
                  -> gr
                  -> [c]
xdfsWith _ _ [] _ = []
xdfsWith _ _ _ g | isEmpty g = []
xdfsWith d f (v:vs) g =
  case match v g of
    Just (c, g') -> f c : xdfsWith d f (d c ++ vs) g'
    Nothing -> xdfsWith d f vs g

dfsWith :: (DecomposableGraph gr)
           => (Context gr -> c)
           -> [Node gr]
           -> gr
           -> [c]
dfsWith = xdfsWith suc'

dfsWith' :: (DecomposableGraph gr, VertexListGraph gr)
            => (Context gr -> c)
            -> gr
            -> [c]
dfsWith' f = fixNodes (dfsWith f)

dfs :: (DecomposableGraph gr)
       => [Node gr] -> gr -> [Node gr]
dfs = dfsWith node'

dfs' :: (DecomposableGraph gr, VertexListGraph gr) => gr -> [Node gr]
dfs' = dfsWith' node'

udfs :: (DecomposableGraph gr, BidirectionalGraph gr)
        => [Node gr] -> gr -> [Node gr]
udfs = xdfsWith neighbors' node'

udfs' :: (DecomposableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
         => gr -> [Node gr]
udfs' = fixNodes udfs

rdfs :: (DecomposableGraph gr, BidirectionalGraph gr)
        => [Node gr] -> gr -> [Node gr]
rdfs = xdfsWith pre' node'

rdfs' :: (DecomposableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
         => gr -> [Node gr]
rdfs' = fixNodes rdfs

xdfWith :: (DecomposableGraph gr)
           => (Context gr -> [Node gr])
           -> (Context gr -> c)
           -> [Node gr]
           -> gr
           -> ([Tree c], gr)
xdfWith _ _ [] g = ([], g)
xdfWith _ _ _ g | isEmpty g = ([], g)
xdfWith d f (v:vs) g =
  case match v g of
    Nothing -> xdfWith d f vs g
    Just (c, g1) ->
      let (ts, g2) = xdfWith d f (d c) g1
          (ts', g3) = xdfWith d f vs g2
      in (Node (f c) ts : ts', g3)

xdffWith :: (DecomposableGraph gr)
            => (Context gr -> [Node gr])
            -> (Context gr -> c)
            -> [Node gr]
            -> gr
            -> [Tree c]
xdffWith d f vs g = fst (xdfWith d f vs g)

dffWith :: (DecomposableGraph gr)
           => (Context gr -> c)
           -> [Node gr]
           -> gr
           -> [Tree c]
dffWith = xdffWith suc'

dffWith' :: (DecomposableGraph gr, VertexListGraph gr)
            => (Context gr -> c)
            -> gr
            -> [Tree c]
dffWith' f = fixNodes (dffWith f)

dff :: (DecomposableGraph gr)
       => [Node gr] -> gr -> [Tree (Node gr)]
dff = dffWith node'

dff' :: (DecomposableGraph gr, VertexListGraph gr) => gr -> [Tree (Node gr)]
dff' = dffWith' node'

udff :: (DecomposableGraph gr, BidirectionalGraph gr) => [Node gr] -> gr -> [Tree (Node gr)]
udff = xdffWith neighbors' node'

udff' :: (DecomposableGraph gr, VertexListGraph gr, BidirectionalGraph gr)
         => gr -> [Tree (Node gr)]
udff' = fixNodes udff

rdff :: (DecomposableGraph gr, BidirectionalGraph gr) => [Node gr] -> gr -> [Tree (Node gr)]
rdff = xdffWith pre' node'

rdff' :: (DecomposableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
         => gr -> [Tree (Node gr)]
rdff' = fixNodes rdff

-- | Return the components in the graph (discarding edges and labels)
components :: (DecomposableGraph gr, VertexListGraph gr, BidirectionalGraph gr)
              => gr -> [[Node gr]]
components = (map preorder) . udff'

-- | Compute the number of components in the graph
noComponents :: (DecomposableGraph gr, VertexListGraph gr, BidirectionalGraph gr)
              => gr -> Int
noComponents = length . components

-- | Test to see whether or not the graph is connected.  The empty
-- graph is not considered connected.
isConnected :: (DecomposableGraph gr, VertexListGraph gr, BidirectionalGraph gr)
              => gr -> Bool
isConnected = (==1) . noComponents

postflatten :: Tree a -> [a]
postflatten (Node v ts) = postflattenF ts ++ [v]

postflattenF :: [Tree a] -> [a]
postflattenF = concatMap postflatten

-- | Topologically sort the nodes in the graph (the graph should be a DAG)
topsort :: (DecomposableGraph gr, VertexListGraph gr, BidirectionalGraph gr)
              => gr -> [Node gr]
topsort = reverse . postflattenF . dff'

-- | Topologically sort the nodes in the graph, just keeping the
-- labels (again, the graph should be a DAG)
topsort' :: (DecomposableGraph gr, VertexListGraph gr, BidirectionalGraph gr)
              => gr -> [NodeLabel gr]
topsort' = reverse . postorderF . dffWith' lab'

-- | Reduce the graph to its strongly-connected components
scc :: (DecomposableGraph gr, VertexListGraph gr, BidirectionalGraph gr)
       => gr -> [[Node gr]]
scc g = map preorder (rdff (topsort g) g)

-- | Compute the nodes reachable from an input node in the graph
reachable :: (DecomposableGraph gr) => Node gr -> gr -> [Node gr]
reachable v g = preorderF (dff [v] g)

-- | Create the condensation of the graph.  There is only a single
-- (unlabeled) edge between strongly-connected components.
condense :: (DecomposableGraph gr1, VertexListGraph gr1, BidirectionalGraph gr1,
             Graph gr2, Node gr2 ~ Int, Ord (Node gr1),
             EdgeLabel gr2 ~ (), NodeLabel gr2 ~ [LNode gr1])
            => gr1 -> gr2
condense g = mkGraph condensedNodes condensedEdges
  where
    sccIds = zip [0..] (scc g)
    nodeToSccMap = foldr buildSccIdMap M.empty sccIds
    sccEdgePairs = foldr (collectSccEdges nodeToSccMap) S.empty sccIds
    condensedNodes = map (sccToNode g) sccIds
    condensedEdges = map (\(s, d) -> LEdge (Edge s d) ()) (S.toList sccEdgePairs)

buildSccIdMap :: (Ord k) => (a, [k]) -> Map k a -> Map k a
buildSccIdMap (componentId, componentNodes) acc =
  foldr (\n a -> M.insert n componentId a) acc componentNodes

-- Find all of the successors to n in the graph.  For each successor s
-- not in the current scc, add the pair (sccId, nodeToSccMap ! s) to
-- the set.
collectSccEdges nodeToSccMap (sccId, ns) =
  foldr (\n a -> S.insert

sccToNode :: (NodeLabel gr2 ~ [LNode gr1], InspectableGraph gr1)
             => gr1 -> (Node gr2, [Node gr1]) -> LNode gr2
sccToNode g (sccId, ns) =
  LNode sccId $ map (labNode' . fromJust . context g) ns
  where
    errMsg = "sccToNode: expected context not found"
    fromJust = maybe (error errMsg) id