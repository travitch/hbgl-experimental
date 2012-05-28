{-# LANGUAGE TypeFamilies, BangPatterns #-}
module Data.Graph.PatriciaTree ( Gr ) where

import Control.Arrow
import Control.DeepSeq
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as IM
import Data.List ( foldl', find, sort )

-- import Data.List.Strict ( List(..), sort )
import Data.Graph.Interface

import Debug.Trace
debug = flip trace

type IntMap = HashMap Int

-- FIXME: Make this a strict list so we can legitimately make deepseq
-- a noop
newtype SList a = SList { unList :: [a] }
                deriving (Show)
newtype Gr a b = Gr { graphRepr :: GraphRep a b }
type GraphRep a b = IntMap (Context' a b)
data Context' a b = Context' !(IntMap (SList b)) !(LNode (Gr a b)) !(IntMap (SList b))
                  deriving (Eq)

instance (Show a, Show b) => Show (Context' a b) where
  show (Context' p n s) = concat ["Context' "
                                 , show p
                                 , " "
                                 , show n
                                 , " "
                                 , show s
                                 ]

instance (Ord a) => Eq (SList a) where
  (SList l1) == (SList l2) = sort l1 == sort l2

instance (NFData a) => NFData (SList a) where
  rnf (SList l) = l `deepseq` ()

instance (NFData a, NFData b) => NFData (Context' a b) where
  rnf (Context' p l s) = () -- p `deepseq` l `deepseq` s `deepseq` ()

instance (NFData n, NFData e) => NFData (Gr n e) where
  rnf (Gr g) = () -- g `deepseq` ()

contextNode :: Context' a b -> LNode (Gr a b)
contextNode (Context' _ a _) = a

instance (Ord e, Eq n) => Graph (Gr n e) where
  type Node (Gr n e) = Int
  type NodeLabel (Gr n e) = n
  type EdgeLabel (Gr n e) = e

  mkGraph ns es =
    let g0 = insNodes ns empty
    in foldl' (flip insEdge) g0 es
  empty = Gr IM.empty
  isEmpty = IM.null . graphRepr

instance (Ord e, Eq n) => InspectableGraph (Gr n e) where
  context (Gr g) n = do
    Context' p ln s <- IM.lookup n g
    return $! Context (toAdj p) ln (toAdj s)

instance (Ord e, Eq n) => DecomposableGraph (Gr n e) where
  match n g = do
    -- This context has all of the predecessors and successors for the
    -- node.
    c@(Context p _ s) <- context g n
    -- Now we need to clear the affected pred/suc edges in the remaining
    -- graph
    let !g1 = IM.delete n (graphRepr g)
        !g2 = clearPred g1 n (map fst s)
        !g3 = clearSucc g2 n (map fst p)
    return (c, Gr g3)

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
  labNodes = map contextNode . IM.elems . graphRepr

instance (Ord e, Eq n) => EdgeListGraph (Gr n e) where
  labEdges (Gr g) = do
    (node, Context' _ _ s) <- IM.toList g
    (next, labels) <- IM.toList s
    label <- unList labels
    return $! LEdge (Edge node next) label

instance (Ord e, Eq n) => AdjacencyMatrix (Gr n e) where
  edgeExists (Gr g) (Edge src dst) = do
    (Context' _ _ s) <- IM.lookup src g
    (_, lbl) <- find ((==dst) . fst) (toAdj s)
    return lbl

instance (Ord e, Eq n) => MutableGraph (Gr n e) where
  (Context p ln@(LNode nid _) s) & (Gr g) =
    let c' = Context' (fromAdj p) ln (fromAdj s)
        !g1 = IM.insert nid c' g
        !g2 = addSucc g1 nid p
        !g3 = addPred g2 nid s
    in Gr g3
  insNode n@(LNode nid _) (Gr g) = g' `seq` Gr g'
    where
      !g' = IM.insert nid (Context' IM.empty n IM.empty) g
  insEdge (LEdge (Edge src dst) l) (Gr g) = g2 `seq` Gr g2
    where
      !g1 = IM.adjust (addSucc' l dst) src g
      !g2 = IM.adjust (addPred' l src) dst g1

addSucc' l dst !(Context' ps l' ss) =
  Context' ps l' (IM.insertWith addLists dst (SList [l]) ss)
addPred' l src !(Context' ps l' ss) =
  Context' (IM.insertWith addLists src (SList [l]) ps) l' ss

instance (Ord e, Eq n) => ComparableGraph (Gr n e) where
  graphEqual (Gr g1) (Gr g2) = g1 == g2



-- Helpers
toAdj :: IntMap (SList (EdgeLabel (Gr a b))) -> Adj (Gr a b)
toAdj = IM.foldrWithKey expand []
  where
    expand n (SList ls) acc = zip (repeat n) ls ++ acc

fromAdj :: (Ord b) => Adj (Gr a b) -> IntMap (SList (EdgeLabel (Gr a b)))
fromAdj = IM.fromListWith addLists . map (second (SList . return))

-- A version of @++@ where order isn't important, so @xs ++ [x]@
-- becomes @x:xs@.  Used when we have to have a function of type @[a]
-- -> [a] -> [a]@ but one of the lists is just going to be a single
-- element (and it isn't possible to tell which).
addLists :: (Ord a) => SList a -> SList a -> SList a
addLists (SList [a]) (SList as)  = SList $ a : as
addLists (SList as)  (SList [a]) = SList $ a : as
addLists (SList xs)  (SList ys)  = SList $ xs ++ ys


addSucc :: (Ord b) => GraphRep a b -> Int -> [(Int, b)] -> GraphRep a b
addSucc g _ []              = g
addSucc g v ((p, l) : rest) = addSucc g' v rest
    where
      !g' = IM.adjust f p g
      f (Context' ps l' ss) = Context' ps l' (IM.insertWith addLists v (SList [l]) ss)


addPred :: (Ord b) => GraphRep a b -> Int -> [(Int, b)] -> GraphRep a b
addPred g _ []              = g
addPred g v ((s, l) : rest) = addPred g' v rest
    where
      !g' = IM.adjust f s g
      f (Context' ps l' ss) = Context' (IM.insertWith addLists v (SList [l]) ps) l' ss

clearSucc :: GraphRep a b -> Int -> [Int] -> GraphRep a b
clearSucc g _ []       = g
clearSucc g v ns =
  foldl' (flip (IM.adjust f)) g ns
  where
    f (Context' ps l ss) = Context' ps l (IM.delete v ss)

clearPred :: GraphRep a b -> Int -> [Int] -> GraphRep a b
clearPred g _ []       = g
clearPred g v ns =
  foldl' (flip (IM.adjust f)) g ns
  where
    f (Context' ps l ss) = Context' (IM.delete v ps) l ss
