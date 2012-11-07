{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Data.Graph.Algorithms.Basic (
   -- * Basic graph operations
--  grev,
--  undir,
--  unlab,
--  gsel,
  -- * Filtering operations
  -- efilter,
  -- elfilter,
  -- * Simple predicates
  -- hasLoop,
  -- isSimple,
  -- * Tree operations
  postorder,
  postorderF,
  preorder,
  preorderF
  ) where

import Data.List ( nub )
import Data.Tree
import Data.Graph.Interface
{-
-- | Reverse the directions of all of the edges in the graph
grev :: (MutableGraph gr, VertexListGraph gr) => gr -> gr
grev = gmap (\(Context p ln s) -> Context s ln p)

-- | Make the graph undirected.  For every edge (A->B)_L (edge from A
-- to B with label L), make a new edge (B->A)_L.
undir :: (MutableGraph gr, VertexListGraph gr) => gr -> gr
undir = gmap (\(Context p ln s) -> let ps = nub (p++s) in Context ps ln ps)

-- | Remove the labels from edges.  This may result in duplicate
-- edges; the resolution of those edges depends on the underlying
-- graph implementation (i.e., whether or not multi-edges are
-- permitted).
unlab :: forall gr1 gr2 .
         (VertexListGraph gr1,
          InspectableGraph gr1,
          MutableGraph gr2,
          LNode gr1 ~ LNode gr2,
          EdgeLabel gr2 ~ ())
         => gr1 -> gr2
unlab = gmap convertContext
  where
    convertContext :: Context gr1 -> Context gr2
    convertContext (Context p ln s) = Context (unlabAdj p) ln (unlabAdj s)
    unlabAdj = map (\(v,_) -> (v,()))

-- | Return all of the contexts for which the predicate evaluates to
-- True
gsel :: (VertexListGraph gr, InspectableGraph gr)
        => (Context gr -> Bool) -- ^ The predicate
        -> gr -- ^ The graph
        -> [Context gr]
gsel p = ufold (\c cs -> if p c then c : cs else cs) []

-- | Filter edges from the graph, keeping those that meet the condition
efilter :: (MutableGraph gr, InspectableGraph gr, VertexListGraph gr)
           => (LEdge gr -> Bool) -- ^ Edge condition
           -> gr -- ^ Input graph
           -> gr
efilter f = ufold cfilter empty
  where
    cfilter (Context p ln@(LNode n _) s) g =
      let p' = filter (\(u, b) -> f (LEdge (Edge u n) b)) p
          s' = filter (\(w, b) -> f (LEdge (Edge n w) b)) s
      in Context p' ln s' & g

-- | Filter edges from the graph based on just the edge label
elfilter :: (MutableGraph gr, InspectableGraph gr, VertexListGraph gr)
            => (EdgeLabel gr -> Bool) -- ^ Edge label condition
            -> gr -- ^ Input graph
            -> gr
elfilter f = efilter (\(LEdge _ el) -> f el)

-- | Test to see if the graph has a self loop on any vertex
hasLoop :: (VertexListGraph gr, InspectableGraph gr) => gr -> Bool
hasLoop = not . null . (gsel (\c -> node' c `elem` suc' c))

-- | Inverse of 'hasLoop'
isSimple :: (VertexListGraph gr, InspectableGraph gr) => gr -> Bool
isSimple = not . hasLoop
-}

-- what is gfold

postorder :: Tree a -> [a]
postorder (Node v ts) = postorderF ts ++ [v]

postorderF :: [Tree a] -> [a]
postorderF = concatMap postorder

preorder :: Tree a -> [a]
preorder = flatten

preorderF :: [Tree a] -> [a]
preorderF = concatMap preorder
