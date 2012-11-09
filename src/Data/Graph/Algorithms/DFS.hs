module Data.Graph.Algorithms.DFS (
  -- * Depth-first search
  xdfsWith,
  dfsWith,
  dfsWith',
  dfs,
  dfs',
  udfs,
  udfs',
  rdfs,
  rdfs',
  -- * Depth-first forests
  xdffWith,
  dffWith,
  dffWith',
  dff,
  dff',
  udff,
  udff',
  rdff,
  rdff',
  -- * Derived
  components,
  noComponents,
  isConnected,
  -- * Topsort
  topsort,
  topsort',
  scc,
  reachable
  ) where

-- import Control.Monad ( filterM, foldM )
-- import Control.Monad.ST
import Data.Tree ( Tree )
import qualified Data.Tree as T

import Data.Graph.Interface
import Data.Graph.Algorithms.Basic

-- Maybe keep the context-based API here?  The allocations required
-- for 'context' are not nearly as bad as 'match'.  Still significant,
-- though.
--
-- If we keep this interface, it may be worth making an alternate
-- implementation (same marking options) that stores Contexts directly
-- so that they can be provided with zero-allocations here

xdfsWith :: (DecomposableGraph gr)
            => (Context gr -> [Vertex])
            -> (Context gr -> c)
            -> [Vertex]
            -> gr
            -> [c]
xdfsWith nextNodes f roots g0
  | isEmpty g0 = []
  | null roots = []
  | otherwise = go roots g0
  where
    go [] _ = []
    go (v:vs) g =
      case match v g of
        Nothing -> go vs g
        Just (c, g') -> f c : go (nextNodes c ++ vs) g'

dfsWith :: (DecomposableGraph gr)
           => (Context gr -> c)
           -> [Vertex]
           -> gr
           -> [c]
dfsWith = xdfsWith suc'

dfsWith' :: (DecomposableGraph gr, VertexListGraph gr)
            => (Context gr -> c)
            -> gr
            -> [c]
dfsWith' f = fixVertices (dfsWith f)

dfs :: (DecomposableGraph gr)
       => [Vertex] -> gr -> [Vertex]
dfs = dfsWith vertex'

dfs' :: (DecomposableGraph gr, VertexListGraph gr)
       => gr -> [Vertex]
dfs' = dfsWith' vertex'

udfs :: (DecomposableGraph gr, BidirectionalGraph gr)
        => [Vertex] -> gr -> [Vertex]
udfs = xdfsWith neighbors' vertex'

udfs' :: (DecomposableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
        => gr -> [Vertex]
udfs' = fixVertices udfs

rdfs :: (DecomposableGraph gr, BidirectionalGraph gr)
        => [Vertex] -> gr -> [Vertex]
rdfs = xdfsWith pre' vertex'

rdfs' :: (DecomposableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
        => gr -> [Vertex]
rdfs' = fixVertices rdfs

xdffWith :: (DecomposableGraph gr)
           => (Context gr -> [Vertex])
           -> (Context gr -> c)
           -> [Vertex]
           -> gr
           -> [Tree c]
xdffWith nextNodes f roots g0
  | null roots = []
  | isEmpty g0 = []
  | otherwise = fst $ go roots g0
  where
    go [] g = ([], g)
    go (v:vs) g =
      case match v g of
        Nothing -> go vs g
        Just (c, g1) ->
          let (ts, g2) = go (nextNodes c) g1
              (ts', g3) = go vs g2
          in (T.Node (f c) ts : ts', g3)

dffWith :: (DecomposableGraph gr)
           => (Context gr -> c)
           -> [Vertex]
           -> gr
           -> [Tree c]
dffWith = xdffWith suc'

dffWith' :: (DecomposableGraph gr, VertexListGraph gr)
            => (Context gr -> c)
            -> gr
            -> [Tree c]
dffWith' f = fixVertices (dffWith f)

dff :: (DecomposableGraph gr)
       => [Vertex] -> gr -> [Tree Vertex]
dff = dffWith vertex'

dff' ::(DecomposableGraph gr, VertexListGraph gr)
       => gr -> [Tree Vertex]
dff' = dffWith' vertex'

udff :: (DecomposableGraph gr, BidirectionalGraph gr)
        => [Vertex] -> gr -> [Tree Vertex]
udff = xdffWith neighbors' vertex'

udff' :: (DecomposableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
        => gr -> [Tree Vertex]
udff' = fixVertices udff

rdff :: (DecomposableGraph gr, BidirectionalGraph gr)
        => [Vertex] -> gr -> [Tree Vertex]
rdff = xdffWith pre' vertex'

rdff' :: (DecomposableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
        => gr -> [Tree Vertex]
rdff' = fixVertices rdff

components :: (DecomposableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
              => gr -> [[Vertex]]
components = (map preorder) . udff'

noComponents :: (DecomposableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
                => gr -> Int
noComponents = length . components

isConnected :: (DecomposableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
               => gr -> Bool
isConnected = (==1) . noComponents

postflatten :: Tree a -> [a]
postflatten (T.Node v ts) = postflattenF ts ++ [v]

postflattenF :: [Tree a] -> [a]
postflattenF = concatMap postflatten

topsort :: (DecomposableGraph gr, VertexListGraph gr)
           => gr -> [Vertex]
topsort = reverse . postflattenF . dff'

topsort' :: (DecomposableGraph gr, VertexListGraph gr)
            => gr -> [VertexLabel gr]
topsort' = reverse . postorderF . dffWith' lab'

scc :: (DecomposableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
       => gr -> [[Vertex]]
scc g = map preorder (rdff (topsort g) g)

reachable :: (DecomposableGraph gr)
             => Vertex -> gr -> [Vertex]
reachable v g = preorderF (dff [v] g)

fixVertices :: (VertexListGraph gr) => ([Vertex] -> gr -> c) -> gr -> c
fixVertices f g = f (vertices g) g