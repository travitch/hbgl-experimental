{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | A compact (immutable) adjacency list graph representation.  Nodes
-- can be of any orderable type.  The primary purpose of this graph is
-- to be compact in memory.
module Data.Graph.Compact where

import Data.List ( foldl', sortBy )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Ord ( comparing )
import Data.Vector ( Vector, (!) )
import qualified Data.Vector as V

import Data.Graph.Interface hiding ( contextNode )

-- | Adjacency information.  This includes the *index into the vector*
-- of an adjacent node, along with the label of the edge.
data Adj' el = Adj' !Int !el

-- | Each context records the *node* and a record of the adjacent
-- nodes.
data Context' n nl el = Context' { adjInc :: [Adj' el]
                                 , contextNode :: !(LNode (Gr n nl el))
                                 , adjOut :: [Adj' el]
                                 }

type GraphRepr n nl el = Vector (Context' n nl el)
-- | The graph is actually an *ordered vector* of node contexts (the
-- ordering is on the Node value).  Access is reasonably efficient
-- (through binary search).
data Gr n nl el = Gr { graphRepr :: !(GraphRepr n nl el) }

-- Use sorted vectors of nodes (Int, Adj) and binary search over
-- them to find targets.  This gives log-time lookups and compact
-- storage.  Adj can be lists or arrays

-- Successor lists could actually be unboxed Int vectors.  Alternatively,
-- short lists could use [], while longer lists could use Vector Int

instance (Eq n, Ord n) => Graph (Gr n nl el) where
  type Node (Gr n nl el) = n
  type NodeLabel (Gr n nl el) = nl
  type EdgeLabel (Gr n nl el) = el

  mkGraph = mkAdjacencyList
  empty = Gr V.empty
  isEmpty = V.null . graphRepr

mkAdjacencyList :: (Ord (Node (Gr n nl el)))
                   => [LNode (Gr n nl el)]
                   -> [LEdge (Gr n nl el)]
                   -> Gr n nl el
mkAdjacencyList ns es = Gr contextList
  where
    -- Sorted list of nodes (this is the order of the contexts in the
    -- vector)
    nodeList = sortBy (comparing unlabelNode) ns
    -- A mapping from nodes to their index into the vector
    ixMap = M.fromList $ zip (map unlabelNode nodeList) [0..]

    (fwdEdgeMap, revEdgeMap) = foldl' (mkEdgeMap ixMap) (M.empty, M.empty) es

    contextList = V.fromList $ map (mkContext fwdEdgeMap revEdgeMap) nodeList

mkContext :: (Ord n)
             => Map (Node (Gr n nl el)) [Adj' el]
             -> Map (Node (Gr n nl el)) [Adj' el]
             -> LNode (Gr n nl el)
             -> Context' n nl el
mkContext fwdEdgeMap revEdgeMap ln@(LNode n _) =
  Context' (adjLookup n revEdgeMap) ln (adjLookup n fwdEdgeMap)
  where
    adjLookup = M.findWithDefault []

mkEdgeMap :: (Ord n)
             => Map (Node (Gr n nl el)) Int
             -> (Map (Node (Gr n nl el)) [Adj' el], Map (Node (Gr n nl el)) [Adj' el])
             -> LEdge (Gr n nl el)
             -> (Map (Node (Gr n nl el)) [Adj' el], Map (Node (Gr n nl el)) [Adj' el])
mkEdgeMap ixMap (fwdMap, revMap) (LEdge (Edge src dst) lbl) = (f', r')
  where
    errMsg = error "No mapping for node in ixMap during graph construction"
    ixLookup = M.findWithDefault errMsg
    f' = M.insertWith' (++) src [(Adj' (ixLookup dst ixMap) lbl)] fwdMap
    r' = M.insertWith' (++) dst [(Adj' (ixLookup src ixMap) lbl)] revMap

binarySearch :: (Eq n, Ord n)
                => GraphRepr n nl el
                -> Node (Gr n nl el)
                -> Maybe (Context' n nl el)
binarySearch v n =
  case V.null v of
    True -> Nothing
    False ->
      let ix = l `div` 2
          elt = v ! ix
      in case unlabelNode (contextNode elt) `compare` n of
        LT -> binarySearch (V.slice ix (V.length v - ix) v) n
        EQ -> Just elt
        GT -> binarySearch (V.slice 0 ix v) n
  where
    l = V.length v

-- Another possible type of graphs: composable (or overlay) graphs.
-- Example: keep all of the global nodes in one graph and overlay
-- local nodes (plus ephemeral edges) in another.  Treat the current
-- graph as the combination of the two.

instance (Eq n, Ord n) => InspectableGraph (Gr n nl el) where
  context (Gr v) n = do
    Context' p ln s <- binarySearch v n
    let p' = map (convertAdj v) p
        s' = map (convertAdj v) s
    return $! Context p' ln s'
    where
      convertAdj vec (Adj' ix lbl) =
        let nod = unlabelNode $ contextNode (vec ! ix)
        in (nod, lbl)
  gelem n (Gr v) = maybe False (const True) (binarySearch v n)

instance (Eq n, Ord n) => IncidenceGraph (Gr n nl el) where
  lout g n =
    case context g n of
      Nothing -> []
      Just (Context _ (LNode src _) s) -> map (toLEdge src) s
    where
      toLEdge src (dest, elbl) = LEdge (Edge src dest) elbl

instance (Eq n, Ord n) => BidirectionalGraph (Gr n nl el) where
  linn g n =
    case context g n of
      Nothing -> []
      Just (Context p (LNode dest _) _) -> map (toLEdge dest) p
    where
      toLEdge dest (src, elbl) = LEdge (Edge src dest) elbl

instance (Eq n, Ord n) => AdjacencyGraph (Gr n nl el)
instance (Eq n, Ord n) => BidirectionalAdjacencyGraph (Gr n nl el)

instance (Eq n, Ord n) => VertexListGraph (Gr n nl el) where
  labNodes (Gr v) = V.foldl' extractNode [] v
    where
      extractNode acc (Context' _ ln _) = ln : acc

instance (Eq n, Ord n) => EdgeListGraph (Gr n nl el) where
  labEdges (Gr v) = concat $ V.foldl (extractEdges v) [] v
    where
      extractEdges vec acc (Context' _ (LNode src _) s) =
        map (toLEdge vec src) s : acc
      toLEdge vec src (Adj' ix elbl) =
        let dest = unlabelNode $ contextNode (vec ! ix)
        in LEdge (Edge src dest) elbl