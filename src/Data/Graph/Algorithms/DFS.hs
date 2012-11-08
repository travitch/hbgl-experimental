{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Monad ( filterM )
import Control.Monad.ST
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

xdfsWith :: forall gr c . (InspectableGraph gr, VertexListGraph gr)
            => (Context gr -> [Vertex])
            -> (Context gr -> c)
            -> [Vertex]
            -> gr
            -> [c]
xdfsWith nextNodes f roots g
  | isEmpty g = []
  | null roots = []
  | otherwise = runST $ do
    m <- newMarker (numVertices g)
    go m [] roots
    where
      go :: VertexMarker gr s -> [c] -> [Vertex] -> ST s [c]
      go _ acc [] = return acc
      go m acc (v:vs) = do
        isM <- isVertexMarked m v
        case isM of
          True -> go m acc vs
          False -> do
            markVertex m v
            case context g v of
              Nothing -> go m acc vs
              Just c -> do
                let nxt = nextNodes c
                nxt' <- filterM (fmap not . isVertexMarked m) nxt
                go m (f c : acc) (nxt' ++ vs)

dfsWith :: (InspectableGraph gr, VertexListGraph gr)
           => (Context gr -> c)
           -> [Vertex]
           -> gr
           -> [c]
dfsWith = xdfsWith suc'

dfsWith' :: (InspectableGraph gr, VertexListGraph gr)
            => (Context gr -> c)
            -> gr
            -> [c]
dfsWith' f = fixVertices (dfsWith f)

dfs :: (InspectableGraph gr, VertexListGraph gr)
       => [Vertex] -> gr -> [Vertex]
dfs = dfsWith vertex'

dfs' :: (InspectableGraph gr, VertexListGraph gr)
       => gr -> [Vertex]
dfs' = dfsWith' vertex'

udfs :: (InspectableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
        => [Vertex] -> gr -> [Vertex]
udfs = xdfsWith neighbors' vertex'

udfs' :: (InspectableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
        => gr -> [Vertex]
udfs' = fixVertices udfs

rdfs :: (InspectableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
        => [Vertex] -> gr -> [Vertex]
rdfs = xdfsWith pre' vertex'

rdfs' :: (InspectableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
        => gr -> [Vertex]
rdfs' = fixVertices rdfs

xdffWith :: forall gr c . (InspectableGraph gr, VertexListGraph gr)
           => (Context gr -> [Vertex])
           -> (Context gr -> c)
           -> [Vertex]
           -> gr
           -> [Tree c]
xdffWith nextNodes f roots g
  | null roots = []
  | isEmpty g = []
  | otherwise = runST $ do
    m <- newMarker (numVertices g)
    go m roots
    where
      go :: VertexMarker gr s -> [Vertex] -> ST s [Tree c]
      go _ [] = return [] -- acc
      go m (v:vs) = do
        isM <- isVertexMarked m v
        case isM of
          True -> go m vs
          False -> do
            markVertex m v
            case context g v of
              Nothing -> go m vs
              Just c -> do
                let nxt = nextNodes c
                nxt' <- filterM (fmap not . isVertexMarked m) nxt
                ts <- go m nxt'
                ts' <- go m vs
                return $ T.Node (f c) ts : ts'

dffWith :: (InspectableGraph gr, VertexListGraph gr)
           => (Context gr -> c)
           -> [Vertex]
           -> gr
           -> [Tree c]
dffWith = xdffWith suc'

dffWith' :: (InspectableGraph gr, VertexListGraph gr)
            => (Context gr -> c)
            -> gr
            -> [Tree c]
dffWith' f = fixVertices (dffWith f)

dff :: (InspectableGraph gr, VertexListGraph gr)
       => [Vertex] -> gr -> [Tree Vertex]
dff = dffWith vertex'

dff' ::(InspectableGraph gr, VertexListGraph gr)
       => gr -> [Tree Vertex]
dff' = dffWith' vertex'

udff :: (InspectableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
        => [Vertex] -> gr -> [Tree Vertex]
udff = xdffWith neighbors' vertex'

udff' :: (InspectableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
        => gr -> [Tree Vertex]
udff' = fixVertices udff

rdff :: (InspectableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
        => [Vertex] -> gr -> [Tree Vertex]
rdff = xdffWith pre' vertex'

rdff' :: (InspectableGraph gr, BidirectionalGraph gr, VertexListGraph gr)
        => gr -> [Tree Vertex]
rdff' = fixVertices rdff

components :: (InspectableGraph gr, VertexListGraph gr, BidirectionalGraph gr)
              => gr -> [[Vertex]]
components = (map preorder) . udff'

noComponents :: (InspectableGraph gr, VertexListGraph gr, BidirectionalGraph gr)
                => gr -> Int
noComponents = length . components

isConnected :: (InspectableGraph gr, VertexListGraph gr, BidirectionalGraph gr)
               => gr -> Bool
isConnected = (==1) . noComponents

postflatten :: Tree a -> [a]
postflatten (T.Node v ts) = postflattenF ts ++ [v]

postflattenF :: [Tree a] -> [a]
postflattenF = concatMap postflatten

topsort :: (InspectableGraph gr, VertexListGraph gr)
           => gr -> [Vertex]
topsort = reverse . postflattenF . dff'

topsort' :: (InspectableGraph gr, VertexListGraph gr)
            => gr -> [VertexLabel gr]
topsort' = reverse . postorderF . dffWith' lab'

scc :: (InspectableGraph gr, VertexListGraph gr, BidirectionalGraph gr)
       => gr -> [[Vertex]]
scc g = map preorder (rdff (topsort g) g)

reachable :: (InspectableGraph gr, VertexListGraph gr)
             => Vertex -> gr -> [Vertex]
reachable v g = preorderF (dff [v] g)

fixVertices :: (VertexListGraph gr) => ([Vertex] -> gr -> c) -> gr -> c
fixVertices f g = f (vertices g) g