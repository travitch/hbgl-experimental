{-# LANGUAGE TypeFamilies #-}
-- | A compact (immutable) adjacency list graph representation.  Nodes
-- can be of any orderable type.  The primary purpose of this graph is
-- to be compact in memory.
module Data.Graph.Compact where

import Data.Vector ( Vector, (!) )
import qualified Data.Vector as V

import Data.Graph.Interface

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

  mkGraph ns es = undefined
  empty = Gr V.empty
  isEmpty = V.null . graphRepr

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
