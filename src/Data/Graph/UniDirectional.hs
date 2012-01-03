{-# LANGUAGE TypeFamilies, KindSignatures #-}
-- | This is a simple graph instance with Int keys and no predecessor
-- edges.
module Data.Graph.UniDirectional ( Gr ) where

-- import Data.Container ( Container )
-- import qualified Data.Container as C
import qualified Data.Foldable as F
import Data.Graph
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM


data Gr (s :: * -> *) n e = Gr { graphRepr :: IntMap (Context (Gr s n e)) }

instance (Container s) => Graph (Gr s n e) where
  type Node (Gr s n e) = Int
  type NodeLabel (Gr s n e) = n
  type EdgeLabel (Gr s n e) = e
  type LinkContainer (Gr s n e) = s
  mkGraph _ _ = undefined
  empty = Gr IM.empty
  isEmpty (Gr g) = IM.null g

instance (Container s) => InspectableGraph (Gr s n e) where
  context (Gr g) n = IM.lookup n g

-- | Since there are no predecessor edges, removing n and its context
-- from the node is sufficient
instance (Container s) => DecomposableGraph (Gr s n e) where
  match n gr@(Gr g) = do
    c <- context gr n
    return (c, Gr (IM.delete n g))

instance (Container s) => IncidenceGraph (Gr s n e) where
  lout g n =
    case context g n of
      Nothing -> []
      Just (Context _ (LNode src _) s) ->
        F.toList $ fmap (\(dst,lbl) -> LEdge (Edge src dst) lbl) s
  -- Slightly more efficient
  out g n =
    case context g n of
      Nothing -> []
      Just (Context _ (LNode src _) s) ->
        F.toList $ fmap (\(dst,_) -> Edge src dst) s

instance (Container s) => AdjacencyGraph (Gr s n e) where
  suc g n =
    case context g n of
      Nothing -> []
      Just (Context _ _ s) ->
        F.toList $ fmap fst s

instance (Container s) => VertexListGraph (Gr s n e) where
  labNodes = map contextNode . IM.elems . graphRepr

instance (Container s) => EdgeListGraph (Gr s n e) where
  labEdges = concatMap extractEdges . IM.elems . graphRepr
    where
      extractEdges (Context _ (LNode src _) s) =
        F.toList $ fmap (mkEdge src) s
      mkEdge src (dst, lbl) = LEdge (Edge src dst) lbl

instance (Container s) => AdjacencyMatrix (Gr s n e) where
  edgeExists (Gr g) (Edge src dst) = do
    (Context _ _ s) <- IM.lookup src g
    (_, lbl) <- F.find ((==dst) . fst) s
    return lbl

{-
-- This implementation does not track predecessors so the context
-- inserted into the map does not contain them.  However, appropriate
-- updates are made to the successor lists of the named predecessors.
instance (Container s) => MutableGraph (Gr s n e) where
  (&) (Context ps ln@(LNode n _) ss) (Gr g) =
    case IM.member n g of
      True -> error "(&) trying to merge a context for a node already in the graph"
      False ->
        let !g' = IM.insert n (Context C.empty ln adj) g
            !g'' = addSucc g' ln (F.toList p)
        in Gr g''
-}

-- IDEA: allow removing nodes without updating the sources of the
-- affected edges.  At the appropriate spots in other operations,
-- filter out edges whose targets do not exit (removing them from the
-- graph when possible).
--
-- Filter from: context, lout, suc, and labEdges (and edgeExists)
-- Update in: match and all of the MutableGraph functions
    {-
addSucc g _ [] = g
addSucc g ln@(LNode n _) ((p, l):rest) = addSucc g' ln rest
  where
    g' = IM.adjust f p g
    f (Context p ln' ss) = Context p ln' (
-}