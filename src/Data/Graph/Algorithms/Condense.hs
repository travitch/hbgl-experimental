{-# LANGUAGE TypeFamilies, BangPatterns, FlexibleContexts #-}
module Data.Graph.Algorithms.Condense ( condense ) where

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Maybe ( fromMaybe )
import Data.Monoid
import Data.Set ( Set )
import qualified Data.Set as S

import Data.Graph.Interface
import Data.Graph.Algorithms.DFS ( scc )

condense :: (InspectableGraph gr1, BidirectionalGraph gr1,
             EdgeListGraph gr1, VertexListGraph gr1,
             Graph gr2, EdgeLabel gr2 ~ (),
             VertexLabel gr2 ~ [(Vertex, VertexLabel gr1)],
             Ord (Edge gr2))
            => gr1 -> gr2
condense g = mkGraph ns es
  where
    sccIds = zip [0..] (scc g)
    nodeToSccMap = foldr buildSccIdMap mempty sccIds
    ns = map (sccToNode g) sccIds
    es = S.toList $ foldr (collectEdges nodeToSccMap) mempty (edges g)

buildSccIdMap :: (Int, [Int]) -> IntMap Int -> IntMap Int
buildSccIdMap (cid, ns) acc =
  foldr (\n a -> IM.insert n cid a) acc ns

sccToNode :: (InspectableGraph gr1) => gr1 -> (a, [Vertex]) -> (a, [(Vertex, VertexLabel gr1)])
sccToNode g (sccId, ns) = (sccId, map toNode ns)
  where
    toNode = labVertex' . fromMaybe errMsg . context g
    errMsg = error "Data.Graph.Algorithms.Condense.scctoNode: Expected context not found"
    labVertex' (Context _ n l _) = (n, l)

-- For each edge (in the original graph), map the source and
-- destination to their SCC id and add the pair to the accumulator
-- set.  These will be the edges for the condensed graph.
collectEdges :: (EdgeLabel gr2 ~ (), Ord (Edge gr2))
                => IntMap Vertex -> Edge gr1 -> Set (Edge gr2) -> Set (Edge gr2)
collectEdges nodeToSccMap (Edge s d _) !acc =
  let Just s' = IM.lookup s nodeToSccMap
      Just d' = IM.lookup d nodeToSccMap
  in S.insert (Edge s' d' ()) acc