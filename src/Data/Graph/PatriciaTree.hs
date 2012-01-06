{-# LANGUAGE TypeFamilies, KindSignatures, BangPatterns, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Graph.PatriciaTree (
  Gr,
  HSGraph,
  SGraph,
  LGraph,
  LHMGraph,
  SHMGraph
  ) where

import Control.DeepSeq
import Data.Foldable ( find, foldl' )
import Data.Hashable
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Monoid

import Data.Graph.Interface
import Data.Graph.LinkStorage

data Context' s n e =
  Context' { adjInc :: s (Node (Gr s n e)) (EdgeLabel (Gr s n e))
           , contextNode :: LNode (Gr s n e)
           , adjOut :: s (Node (Gr s n e)) (EdgeLabel (Gr s n e))
           }

instance (Eq (LNode (Gr s n e)), Eq (s Int e)) => Eq (Context' s n e) where
  (Context' _ n1 s1) == (Context' _ n2 s2) =
    n1 == n2 && s1 == s2
instance (NFData (s Int e), NFData (LNode (Gr s n e))) => NFData (Context' s n e) where
  rnf c@(Context' s ln p) = s `deepseq` p `deepseq` ln `deepseq` c `seq` ()

-- The merge just takes the label of the first context.
mergeContext :: Monoid (s Int e) => Context' s n e -> Context' s n e -> Context' s n e
mergeContext (Context' p1 ln s1) (Context' p2 _ s2) =
  Context' (mappend p1 p2) ln (mappend s1 s2)

type GraphRepr s n e = IntMap (Context' s n e)
-- | The base graph type, parameterized by link storage type.
data Gr (s :: * -> * -> *) n e = Gr { graphRepr :: GraphRepr s n e }

-- FIXME: Try to use type families to make Contex' = Context if s == []

-- | A graph that stores edges in a hash set
type HSGraph = Gr HashSetPair
-- | A graph that stores edges in a normal set
type SGraph = Gr SetPair
-- | A graph that stores edges in a list
type LGraph = Gr ListPair
-- | A graph that stores edges using a lazy hashmap
type LHMGraph = Gr LHMap
-- | A graph that stores edges using a strict hashmap
type SHMGraph = Gr SHMap


-- If we instantiate Gr with a HashSet for storage, the edge sets have
-- canonical forms and we can have a relatively cheap graph equality
-- check.
instance (Hashable e, Eq e, Eq (LNode (Gr HashSetPair n e)),
          Eq (HashSetPair Int e))
         => ComparableGraph (Gr HashSetPair n e) where
  graphEqual (Gr g1) (Gr g2) = g1 == g2
instance (Hashable e, Eq e) => Monoid (Gr HashSetPair n e) where
  mempty = Gr (IM.empty)
  mappend (Gr g1) (Gr g2) = Gr (IM.unionWith mergeContext g1 g2)
{-
instance (NFData (LNode (Gr HashSetPair n e))) => NFData (Gr HashSetPair n e) where
  rnf g@(Gr r) = r `deepseq` g `seq` ()
-}
instance (LinkStorage s Int e, Eq (LNode (Gr s n e)), Eq (s Int e))
         => Graph (Gr s n e) where
  type Node (Gr s n e) = Int
  type NodeLabel (Gr s n e) = n
  type EdgeLabel (Gr s n e) = e

  mkGraph ns es =
    let g0 = insNodes ns empty
    in foldl' (flip insEdge) g0 es
  empty = Gr IM.empty
  isEmpty = IM.null . graphRepr

instance (LinkStorage s Int e, Eq (LNode (Gr s n e)), Eq (s Int e))
         => InspectableGraph (Gr s n e) where
  context g n = do
    Context' p ln s <- IM.lookup n (graphRepr g)
    return $! Context (linkToList p) ln (linkToList s)

instance (LinkStorage s Int e, Eq (LNode (Gr s n e)), Eq (s Int e))
         => DecomposableGraph (Gr s n e) where
  match n g = do
    -- This context has all of the predecessors and successors for the
    -- node.
    c@(Context p _ s) <- context g n
    -- Now we need to clear the affected pred/suc edges in the remaining
    -- graph
    let !g1 = IM.delete n (graphRepr g)
        !g2 = clearPred g1 n s
        !g3 = clearSucc g2 n p
    return (c, Gr g3)

instance (LinkStorage s Int e, Eq (LNode (Gr s n e)), Eq (s Int e))
         => IncidenceGraph (Gr s n e) where
  lout g n =
    case context g n of
      Nothing -> []
      Just (Context _ (LNode src _) s) ->
        map (\(dst,lbl) -> LEdge (Edge src dst) lbl) s
  out g n =
    case context g n of
      Nothing -> []
      Just (Context _ (LNode src _) s) ->
        map (\(dst,_) -> Edge src dst) s

instance (LinkStorage s Int e, Eq (LNode (Gr s n e)), Eq (s Int e))
         => BidirectionalGraph (Gr s n e) where
  linn g n =
    case context g n of
      Nothing -> []
      Just (Context p (LNode dst _) _) ->
        map (\(src, lbl) -> LEdge (Edge src dst) lbl) p
  inn g n =
    case context g n of
      Nothing -> []
      Just (Context p (LNode dst _) _) ->
        map (\(src,_) -> Edge src dst) p

instance (LinkStorage s Int e, Eq (LNode (Gr s n e)), Eq (s Int e))
         => AdjacencyGraph (Gr s n e) where
  suc g n =
    case context g n of
      Nothing -> []
      Just (Context _ _ s) -> map fst s

instance (LinkStorage s Int e, Eq (LNode (Gr s n e)), Eq (s Int e))
         => BidirectionalAdjacencyGraph (Gr s n e) where
  pre g n =
    case context g n of
      Nothing -> []
      Just (Context p _ _) -> map fst p

instance (LinkStorage s Int e, Eq (LNode (Gr s n e)), Eq (s Int e))
         => VertexListGraph (Gr s n e) where
  labNodes = map contextNode . IM.elems . graphRepr

instance (LinkStorage s Int e, Eq (LNode (Gr s n e)), Eq (s Int e))
         => EdgeListGraph (Gr s n e) where
  labEdges = concatMap extractEdges . IM.elems . graphRepr
    where
      extractEdges (Context' _ (LNode src _) s) = linkFold (mkEdge src) [] s
      mkEdge src dst lbl acc = LEdge (Edge src dst) lbl : acc

instance (LinkStorage s Int e, Eq (LNode (Gr s n e)), Eq (s Int e))
         => AdjacencyMatrix (Gr s n e) where
  edgeExists (Gr g) (Edge src dst) = do
    (Context' _ _ s) <- IM.lookup src g
    (_, lbl) <- find ((==dst) . fst) (linkToList s)
    return lbl

instance (LinkStorage s Int e, Eq (LNode (Gr s n e)), Eq (s Int e))
         => MutableGraph (Gr s n e) where
  (Context p ln@(LNode nid _) s) & (Gr g) =
    let c' = Context' (linkFromList p) ln (linkFromList s)
        !g1 = IM.insert nid c' g
        !g2 = addSucc g1 nid p
        !g3 = addPred g2 nid s
    in Gr g3

-- | FIXME: Convert these helpers to take collections of s n e and use
-- linkFold to modify the graph
addSucc :: (LinkStorage s Int e)
           => GraphRepr s n e
           -> Node (Gr s n e)
           -> [(Node (Gr s n e), EdgeLabel (Gr s n e))]
           -> GraphRepr s n e
addSucc g _ [] = g
addSucc !g dst ((src, lbl) : rest) = addSucc g' dst rest
  where
    g' = IM.adjust f src g
    f (Context' p ln s) = Context' p ln (linkInsert dst lbl s)

addPred :: (LinkStorage s Int e)
           => GraphRepr s n e
           -> Node (Gr s n e)
           -> [(Node (Gr s n e), EdgeLabel (Gr s n e))]
           -> GraphRepr s n e
addPred g _ [] = g
addPred !g src ((dst, lbl) : rest) = addPred g' src rest
  where
    g' = IM.adjust f dst g
    f (Context' p ln s) = Context' (linkInsert src lbl p) ln s

clearSucc :: (LinkStorage s Int e)
             => GraphRepr s n e
             -> Node (Gr s n e)
             -> [(Node (Gr s n e), EdgeLabel (Gr s n e))]
             -> GraphRepr s n e
clearSucc g _ [] = g
clearSucc !g dst ((src,_) : rest) = clearSucc g' dst rest
  where
    g' = IM.adjust f src g
    f (Context' p ln s) = Context' p ln (linkDeleteAll dst s)

clearPred :: (LinkStorage s Int e)
             => GraphRepr s n e
             -> Node (Gr s n e)
             -> [(Node (Gr s n e), EdgeLabel (Gr s n e))]
             -> GraphRepr s n e
clearPred g _ [] = g
clearPred !g src ((dst,_) : rest) = clearPred g' src rest
  where
    g' = IM.adjust f dst g
    f (Context' p ln s) = Context' (linkDeleteAll src p) ln s
