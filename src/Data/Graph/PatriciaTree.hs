{-# LANGUAGE TypeFamilies, BangPatterns #-}
module Data.Graph.PatriciaTree ( Gr ) where

import Control.Arrow
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.List ( foldl', find )

import Data.Graph.Interface

newtype Gr a b = Gr { graphRepr :: GraphRep a b }
type GraphRep a b = IntMap (Context' a b)
data Context' a b = Context' !(IntMap [b]) !(LNode (Gr a b)) !(IntMap [b])

contextNode :: Context' a b -> LNode (Gr a b)
contextNode (Context' _ a _) = a

instance (Eq e, Eq n) => Graph (Gr n e) where
  type Node (Gr n e) = Int
  type NodeLabel (Gr n e) = n
  type EdgeLabel (Gr n e) = e

  mkGraph ns es =
    let g0 = insNodes ns empty
    in foldl' (flip insEdge) g0 es
  empty = Gr IM.empty
  isEmpty = IM.null . graphRepr

instance (Eq e, Eq n) => InspectableGraph (Gr n e) where
  context g n = do
    Context' p ln s <- IM.lookup n (graphRepr g)
    return $! Context (toAdj p) ln (toAdj s)

instance (Eq e, Eq n) => DecomposableGraph (Gr n e) where
  match n g = do
    -- This context has all of the predecessors and successors for the
    -- node.
    c@(Context p _ s) <- context g n
    -- Now we need to clear the affected pred/suc edges in the remaining
    -- graph
    let !g1 = IM.delete n (graphRepr g)
        !p' = IM.delete n (fromAdj p)
        !s' = IM.delete n (fromAdj s)
        !g2 = clearPred g1 n (IM.keys s')
        !g3 = clearSucc g2 n (IM.keys p')
    return (c, Gr g3)

instance (Eq e, Eq n) => IncidenceGraph (Gr n e) where
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

instance (Eq e, Eq n) => BidirectionalGraph (Gr n e) where
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

instance (Eq e, Eq n) => AdjacencyGraph (Gr n e) where
  suc g n =
    case context g n of
      Nothing -> []
      Just (Context _ _ s) -> map fst s

instance (Eq e, Eq n) => BidirectionalAdjacencyGraph (Gr n e) where
  pre g n =
    case context g n of
      Nothing -> []
      Just (Context p _ _) -> map fst p

instance (Eq e, Eq n) => VertexListGraph (Gr n e) where
  labNodes = map contextNode . IM.elems . graphRepr

instance (Eq e, Eq n) => EdgeListGraph (Gr n e) where
  labEdges (Gr g) = do
    (node, Context' _ _ s) <- IM.toList g
    (next, labels) <- IM.toList s
    label <- labels
    return $! LEdge (Edge node next) label

instance (Eq e, Eq n) => AdjacencyMatrix (Gr n e) where
  edgeExists (Gr g) (Edge src dst) = do
    (Context' _ _ s) <- IM.lookup src g
    (_, lbl) <- find ((==dst) . fst) (toAdj s)
    return lbl

instance (Eq e, Eq n) => MutableGraph (Gr n e) where
  (Context p ln@(LNode nid _) s) & (Gr g) =
    let c' = Context' (fromAdj p) ln (fromAdj s)
        !g1 = IM.insert nid c' g
        !g2 = addSucc g1 nid p
        !g3 = addPred g2 nid s
    in Gr g3


-- Helpers
toAdj :: IntMap [EdgeLabel (Gr a b)] -> Adj (Gr a b)
toAdj = concatMap expand . IM.toList
  where
    expand (n,ls) = map ((,) n) ls


fromAdj :: Adj (Gr a b) -> IntMap [EdgeLabel (Gr a b)]
fromAdj = IM.fromListWith addLists . map (second return)

-- A version of @++@ where order isn't important, so @xs ++ [x]@
-- becomes @x:xs@.  Used when we have to have a function of type @[a]
-- -> [a] -> [a]@ but one of the lists is just going to be a single
-- element (and it isn't possible to tell which).
addLists :: [a] -> [a] -> [a]
addLists [a] as  =
  let newl = a : as
  in length newl `seq` newl
addLists as  [a] =
  let newl = a : as
  in length newl `seq` newl
addLists xs  ys  =
  let newl = xs ++ ys
  in length newl `seq` newl

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

addSucc :: GraphRep a b -> Int -> [(Int, b)] -> GraphRep a b
addSucc g _ []              = g
addSucc g v ((p, l) : rest) = addSucc g' v rest
    where
      !g' = IM.adjust f p g
      f (Context' ps l' ss) = Context' ps l' (IM.insertWith addLists v [l] ss)


addPred :: GraphRep a b -> Int -> [(Int, b)] -> GraphRep a b
addPred g _ []              = g
addPred g v ((s, l) : rest) = addPred g' v rest
    where
      !g' = IM.adjust f s g
      f (Context' ps l' ss) = Context' (IM.insertWith addLists v [l] ps) l' ss


clearSucc :: GraphRep a b -> Int -> [Int] -> GraphRep a b
clearSucc g _ []       = g
clearSucc g v (p:rest) = clearSucc g' v rest
    where
      !g' = IM.adjust f p g
      f (Context' ps l ss) = Context' ps l (IM.delete v ss)


clearPred :: GraphRep a b -> Int -> [Int] -> GraphRep a b
clearPred g _ []       = g
clearPred g v (s:rest) = clearPred g' v rest
    where
      !g' = IM.adjust f s g
      f (Context' ps l ss) = Context' (IM.delete v ps) l ss
