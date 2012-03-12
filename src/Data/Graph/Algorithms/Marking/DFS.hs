{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | This module defines variants of depth-first search algorithms
-- graphs that are *not* decomposable.  These algorithms use node marking
-- and require Node types to be orderable.
module Data.Graph.Algorithms.Marking.DFS (
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
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.Tree ( Tree )
import qualified Data.Tree as T
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
xdfsWith :: (InspectableGraph gr, Ord (Node gr))
            => (Context gr -> [Node gr])
            -> (Context gr -> c)
            -> [Node gr]
            -> gr
            -> [c]
xdfsWith _ _ _ g | isEmpty g = []
xdfsWith dirFunc f vs g = go [] vs S.empty
  where
    go acc [] _ = acc
    go acc (n:ns) visited =
      case n `S.member` visited of
        True -> go acc ns visited
        False ->
          case context g n of
            Just c -> go (f c : acc) (dirFunc c ++ ns) (S.insert n visited)
            Nothing -> go acc ns visited

dfsWith :: (InspectableGraph gr, Ord (Node gr))
           => (Context gr -> c)
           -> [Node gr]
           -> gr
           -> [c]
dfsWith = xdfsWith suc'

dfsWith' :: (InspectableGraph gr, Ord (Node gr), VertexListGraph gr)
            => (Context gr -> c)
            -> gr
            -> [c]
dfsWith' f = fixNodes (dfsWith f)

dfs :: (InspectableGraph gr, Ord (Node gr))
       => [Node gr] -> gr -> [Node gr]
dfs = dfsWith node'

dfs' :: (InspectableGraph gr, Ord (Node gr), VertexListGraph gr) => gr -> [Node gr]
dfs' = dfsWith' node'

udfs :: (InspectableGraph gr, Ord (Node gr), BidirectionalGraph gr)
        => [Node gr] -> gr -> [Node gr]
udfs = xdfsWith neighbors' node'

udfs' :: (InspectableGraph gr, Ord (Node gr), BidirectionalGraph gr, VertexListGraph gr)
         => gr -> [Node gr]
udfs' = fixNodes udfs

rdfs :: (InspectableGraph gr, Ord (Node gr), BidirectionalGraph gr)
        => [Node gr] -> gr -> [Node gr]
rdfs = xdfsWith pre' node'

rdfs' :: (InspectableGraph gr, Ord (Node gr), BidirectionalGraph gr, VertexListGraph gr)
         => gr -> [Node gr]
rdfs' = fixNodes rdfs

xdfWith :: (InspectableGraph gr, Ord (Node gr))
           => (Context gr -> [Node gr])
           -> (Context gr -> c)
           -> [Node gr]
           -> gr
           -> [Tree c]
xdfWith _ _ [] _ = []
xdfWith _ _ _ g | isEmpty g = []
xdfWith dirFunc f vs g = fst $ go [] vs S.empty
  where
    go acc [] visited = (acc, visited)
    go acc (n:ns) visited =
      case n `S.member` visited of
        True -> go acc ns visited
        False ->
          case context g n of
            Nothing -> go acc ns visited
            Just c ->
              let vis1 = S.insert n visited
                  (ts, vis2) = go acc (dirFunc c) vis1
              in go (T.Node (f c) ts : acc) ns vis2

xdffWith :: (InspectableGraph gr, Ord (Node gr))
            => (Context gr -> [Node gr])
            -> (Context gr -> c)
            -> [Node gr]
            -> gr
            -> [Tree c]
xdffWith d f vs g = xdfWith d f vs g

dffWith :: (InspectableGraph gr, Ord (Node gr))
           => (Context gr -> c)
           -> [Node gr]
           -> gr
           -> [Tree c]
dffWith = xdffWith suc'

dffWith' :: (InspectableGraph gr, Ord (Node gr), VertexListGraph gr)
            => (Context gr -> c)
            -> gr
            -> [Tree c]
dffWith' f = fixNodes (dffWith f)

dff :: (InspectableGraph gr, Ord (Node gr))
       => [Node gr] -> gr -> [Tree (Node gr)]
dff = dffWith node'

dff' :: (InspectableGraph gr, Ord (Node gr), VertexListGraph gr) => gr -> [Tree (Node gr)]
dff' = dffWith' node'

udff :: (InspectableGraph gr, Ord (Node gr), BidirectionalGraph gr) => [Node gr] -> gr -> [Tree (Node gr)]
udff = xdffWith neighbors' node'

udff' :: (InspectableGraph gr, Ord (Node gr), VertexListGraph gr, BidirectionalGraph gr)
         => gr -> [Tree (Node gr)]
udff' = fixNodes udff

rdff :: (InspectableGraph gr, Ord (Node gr), BidirectionalGraph gr) => [Node gr] -> gr -> [Tree (Node gr)]
rdff = xdffWith pre' node'

rdff' :: (InspectableGraph gr, Ord (Node gr), BidirectionalGraph gr, VertexListGraph gr)
         => gr -> [Tree (Node gr)]
rdff' = fixNodes rdff

-- | Return the components in the graph (discarding edges and labels)
components :: (InspectableGraph gr, Ord (Node gr), VertexListGraph gr, BidirectionalGraph gr)
              => gr -> [[Node gr]]
components = (map preorder) . udff'

-- | Compute the number of components in the graph
noComponents :: (InspectableGraph gr, Ord (Node gr), VertexListGraph gr, BidirectionalGraph gr)
              => gr -> Int
noComponents = length . components

-- | Test to see whether or not the graph is connected.  The empty
-- graph is not considered connected.
isConnected :: (InspectableGraph gr, Ord (Node gr), VertexListGraph gr, BidirectionalGraph gr)
              => gr -> Bool
isConnected = (==1) . noComponents

postflatten :: Tree a -> [a]
postflatten (T.Node v ts) = postflattenF ts ++ [v]

postflattenF :: [Tree a] -> [a]
postflattenF = concatMap postflatten

-- | Topologically sort the nodes in the graph (the graph should be a DAG).
-- The leaves appear at the beginning of the result list.
topsort :: (InspectableGraph gr, Ord (Node gr), VertexListGraph gr, BidirectionalGraph gr)
              => gr -> [Node gr]
topsort = reverse . postflattenF . dff'

-- | Topologically sort the nodes in the graph, just keeping the
-- labels (again, the graph should be a DAG)
topsort' :: (InspectableGraph gr, Ord (Node gr), VertexListGraph gr, BidirectionalGraph gr)
              => gr -> [NodeLabel gr]
topsort' = reverse . postorderF . dffWith' lab'

-- | Reduce the graph to its strongly-connected components
scc :: (InspectableGraph gr, Ord (Node gr), VertexListGraph gr, BidirectionalGraph gr)
       => gr -> [[Node gr]]
scc g = map preorder (rdff (topsort g) g)

-- | Compute the nodes reachable from an input node in the graph
reachable :: (InspectableGraph gr, Ord (Node gr)) => Node gr -> gr -> [Node gr]
reachable v g = preorderF (dff [v] g)


-- | Create the condensation of the graph.  There is only a single
-- (unlabeled) edge between strongly-connected components.
condense :: (AdjacencyGraph gr1, VertexListGraph gr1, BidirectionalGraph gr1,
             Graph gr2, Node gr2 ~ Int, Ord (Node gr1),
             EdgeLabel gr2 ~ (), NodeLabel gr2 ~ [LNode gr1])
            => gr1 -> gr2
condense g = mkGraph condensedNodes condensedEdges
  where
    sccIds = zip [0..] (scc g)
    nodeToSccMap = foldr buildSccIdMap M.empty sccIds
    sccEdgePairs = foldr (collectSccEdges g nodeToSccMap) HS.empty sccIds
    condensedNodes = map (sccToNode g) sccIds
    condensedEdges = map (\(s, d) -> LEdge (Edge s d) ()) (HS.toList sccEdgePairs)

buildSccIdMap :: (Ord k) => (a, [k]) -> Map k a -> Map k a
buildSccIdMap (componentId, componentNodes) acc =
  foldr (\n a -> M.insert n componentId a) acc componentNodes

-- Find all of the successors to n in the graph.  For each successor s
-- not in the current scc, add the pair (sccId, nodeToSccMap ! s) to
-- the set.
collectSccEdges :: (AdjacencyGraph gr, Ord (Node gr))
                   => gr
                   -> Map (Node gr) Int
                   -> (Int, [Node gr])
                   -> HashSet (Int, Int)
                   -> HashSet (Int, Int)
collectSccEdges g nodeToSccMap (sccId, ns) acc =
  foldr addEdges acc ns
  where
    addEdges n a =
      -- Successors to n in g that are not in the same SCC
      let origSuccs = filter ((/=sccId) . (nodeToSccMap!)) $ suc g n
      in foldr (\origSuc s -> HS.insert (sccId, nodeToSccMap ! origSuc) s) a origSuccs

sccToNode :: (NodeLabel gr2 ~ [LNode gr1], InspectableGraph gr1)
             => gr1 -> (Node gr2, [Node gr1]) -> LNode gr2
sccToNode g (sccId, ns) =
  LNode sccId $ map (labNode' . fromJust . context g) ns
  where
    errMsg = "sccToNode: expected context not found"
    fromJust = maybe (error errMsg) id
