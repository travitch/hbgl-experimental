{-# LANGUAGE TypeFamilies, BangPatterns #-}
-- | This is a graph implementation similar to the PatriciaTree from
-- fgl.  It is optimized for *traversal* instead of fast creation
-- (creation is still reasonably fast).  The specific use case is to
-- make DFS traversals (which rely on many repeated calls to 'match')
-- efficient.
--
-- The key change is that matching (removing) a node from the graph
-- does not update successor and predecessor lists.  These updates
-- were taking most of the time and memory in client applications.
-- Instead, removed nodes are added to a hash set (and are removed
-- from the outer hash table).  Match now only makes two allocations:
-- a new hash table without the deleted node and an updated deleted
-- nodes set.
--
-- The deleted node set is used to filter the results of suc/pre
-- operations (and other relevant operations) to maintain correctness.
-- The downside is that the deleted edges live on in the adjacency
-- lists.  For that reason, this structure is mostly only suitable for
-- graphs that are built up but do not have many edges permanently
-- removed (just transiently removed for match traversals).
--
-- A key change in the internal representation is a move from IntMaps
-- to plain lists of pairs to store adjacency information.  This is
-- more compact (which is beneficial to large graphs), but it does
-- slow down insertions somewhat since the lists are treated as
-- ordered sets, so insertions are linear in the number of
-- adjacencies, rather than logarithmic.
module Data.Graph.LazyHAMT ( Gr ) where

import Control.DeepSeq
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as IM
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS
import Data.List ( foldl', find )
import Data.List.Ordered ( insertSet )
import Data.Monoid

import Data.Graph.Interface

type GraphRep a b = HashMap Int (Context (Gr a b))
data Gr a b = Gr { graphRepr :: !(GraphRep a b)
                 , deletedNodes :: !(HashSet Int)
                 }

instance (NFData n, NFData e) => NFData (Gr n e) where
  rnf (Gr _ _) = () -- g `deepseq` ()

instance (Ord e, Eq n) => Graph (Gr n e) where
  type Node (Gr n e) = Int
  type NodeLabel (Gr n e) = n
  type EdgeLabel (Gr n e) = e

  mkGraph ns es =
    let g0 = insNodes ns empty
    in foldl' (flip insEdge) g0 es
  empty = Gr mempty mempty
  isEmpty = IM.null . graphRepr

instance (Ord e, Eq n) => InspectableGraph (Gr n e) where
  context (Gr g _) n = IM.lookup n g

instance (Ord e, Eq n) => DecomposableGraph (Gr n e) where
  match n g = do
    -- This context has all of the predecessors and successors for the
    -- node.
    c <- context g n
    -- Now we need to clear the affected pred/suc edges in the remaining
    -- graph
    let !g1 = IM.delete n (graphRepr g)
    return (c, Gr g1 (HS.insert n (deletedNodes g)))

instance (Ord e, Eq n) => IncidenceGraph (Gr n e) where
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

instance (Ord e, Eq n) => BidirectionalGraph (Gr n e) where
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

instance (Ord e, Eq n) => AdjacencyGraph (Gr n e) where
  suc g n =
    case context g n of
      Nothing -> []
      Just (Context _ _ s) -> map fst s

instance (Ord e, Eq n) => BidirectionalAdjacencyGraph (Gr n e) where
  pre g n =
    case context g n of
      Nothing -> []
      Just (Context p _ _) -> map fst p

instance (Ord e, Eq n) => VertexListGraph (Gr n e) where
  labNodes g =
    filter (not . (`HS.member` deletedNodes g) . unlabelNode) $ map contextNode $ IM.elems $ graphRepr g

instance (Ord e, Eq n) => EdgeListGraph (Gr n e) where
  labEdges (Gr g dns) =
    foldr collectEdges [] (IM.toList g)
    where
      collectEdges (node, Context _ _ s) acc =
        case node `HS.member` dns of
          True -> acc
          False -> foldr (collectEdge node) acc s
      collectEdge node (next, label) acc =
        case next `HS.member` dns of
          True -> acc
          False -> LEdge (Edge node next) label : acc

instance (Ord e, Eq n) => AdjacencyMatrix (Gr n e) where
  edgeExists (Gr g dns) (Edge src dst) =
    case src `HS.member` dns || dst `HS.member` dns of
      True -> Nothing
      False -> do
        (Context _ _ s) <- IM.lookup src g
        (_, lbl) <- find ((==dst) . fst) s
        return lbl

instance (Ord e, Eq n) => MutableGraph (Gr n e) where
  c@(Context p (LNode nid _) s) & (Gr g dns) =
    let !g1 = IM.insert nid c g
        !g2 = addSucc g1 nid p
        !g3 = addPred g2 nid s
    in Gr g3 dns
  insNode n@(LNode nid _) (Gr g dns) = g' `seq` Gr g' dns
    where
      !g' = IM.insert nid (Context mempty n mempty) g
  insEdge (LEdge (Edge src dst) l) (Gr g dns) = g2 `seq` Gr g2 dns
    where
      !g1 = IM.adjust (addSucc' l dst) src g
      !g2 = IM.adjust (addPred' l src) dst g1

addSucc' :: (Ord e) => EdgeLabel (Gr n e) -> Node (Gr n e) -> Context (Gr n e) -> Context (Gr n e)
addSucc' l dst !(Context ps l' ss) =
  Context ps l' (insertSet (dst, l) ss)
addPred' :: (Ord e) => EdgeLabel (Gr n e) -> Node (Gr n e) -> Context (Gr n e) -> Context (Gr n e)
addPred' l src !(Context ps l' ss) =
  Context (insertSet (src, l) ps) l' ss

instance (Ord e, Eq n) => ComparableGraph (Gr n e) where
  graphEqual (Gr g1 _) (Gr g2 _) = g1 == g2



-- Helpers

addSucc :: (Ord b) => GraphRep a b -> Int -> [(Int, b)] -> GraphRep a b
addSucc g _ []              = g
addSucc g v ((p, l) : rest) = addSucc g' v rest
    where
      !g' = IM.adjust f p g
      f (Context ps l' ss) = Context ps l' (insertSet (v, l) ss)

addPred :: (Ord b) => GraphRep a b -> Int -> [(Int, b)] -> GraphRep a b
addPred g _ []              = g
addPred g v ((s, l) : rest) = addPred g' v rest
    where
      !g' = IM.adjust f s g
      f (Context ps l' ss) = Context (insertSet (v, l) ps) l' ss
