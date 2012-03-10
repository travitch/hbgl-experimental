{-# LANGUAGE TypeFamilies, BangPatterns #-}
module Data.Graph.HAMT ( Gr ) where

import Control.Arrow ( second )
import Control.DeepSeq
import Data.Hashable
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.List ( find, foldl', insert )

import Data.Graph.Interface

newtype Gr n nl el = Gr { graphRepr :: GraphRep n nl el }
type GraphRep n nl el = HashMap n (Context' n nl el)
data Context' n nl el = Context' !(HashMap n [el]) !(LNode (Gr n nl el)) !(HashMap n [el])

instance (Eq n, Eq nl, Eq el) => Eq (Context' n nl el) where
  (Context' _ n1 s1) == (Context' _ n2 s2) = n1 == n2 && s1 == s2

instance (NFData n, NFData nl, NFData el) => NFData (Context' n nl el) where
  rnf c@(Context' p n s) = p `deepseq` n `deepseq` s `deepseq` c `seq` ()

instance (NFData n, NFData nl, NFData el) => NFData (Gr n nl el) where
  rnf (Gr g) = g `deepseq` ()

contextNode :: Context' n nl el -> LNode (Gr n nl el)
contextNode (Context' _ ln _) = ln

instance (Hashable n, Eq n, Eq nl, Ord el) => Graph (Gr n nl el) where
  type Node (Gr n nl el) = n
  type NodeLabel (Gr n nl el) = nl
  type EdgeLabel (Gr n nl el) = el

  mkGraph ns es =
    let g0 = insNodes ns empty
    in foldl' (flip insEdge) g0 es
  empty = Gr HM.empty
  isEmpty = HM.null . graphRepr

instance (Hashable n, Eq n, Eq nl, Ord el) => InspectableGraph (Gr n nl el) where
  context g n = do
    Context' p ln s <- HM.lookup n (graphRepr g)
    return $! Context (toAdj p) ln (toAdj s)

instance (Hashable n, Eq n, Eq nl, Ord el) => DecomposableGraph (Gr n nl el) where
  match n g = do
    c@(Context p _ s) <- context g n
    let !g1 = HM.delete n (graphRepr g)
        !p' = HM.delete n (fromAdj p)
        !s' = HM.delete n (fromAdj s)
        !g2 = clearPred g1 n (HM.keys s')
        !g3 = clearSucc g2 n (HM.keys p')
    return (c, Gr g3)

instance (Hashable n, Eq n, Eq nl, Ord el) => IncidenceGraph (Gr n nl el) where
  lout g n =
    case context g n of
      Nothing -> []
      Just (Context _ (LNode src _) s) ->
        map (\(dst, lbl) -> LEdge (Edge src dst) lbl) s
  out g n =
    case context g n of
      Nothing -> []
      Just (Context _ (LNode src _) s) ->
        map (\(dst, _) -> Edge src dst) s

instance (Hashable n, Eq n, Eq nl, Ord el) => BidirectionalGraph (Gr n nl el) where
  linn g n =
    case context g n of
      Nothing -> []
      Just (Context p (LNode dst _) _) ->
        map (\(src, lbl) -> LEdge (Edge src dst) lbl) p
  inn g n =
    case context g n of
      Nothing -> []
      Just (Context p (LNode dst _) _) ->
        map (\(src, _) -> Edge src dst) p

instance (Hashable n, Eq n, Eq nl, Ord el) => AdjacencyGraph (Gr n nl el) where
  suc g n =
    case context g n of
      Nothing -> []
      Just (Context _ _ s) -> map fst s

instance (Hashable n, Eq n, Eq nl, Ord el) => BidirectionalAdjacencyGraph (Gr n nl el) where
  pre g n =
    case context g n of
      Nothing -> []
      Just (Context p _ _) -> map fst p

instance (Hashable n, Eq n, Eq nl, Ord el) => VertexListGraph (Gr n nl el) where
  labNodes = map contextNode . HM.elems . graphRepr

instance (Hashable n, Eq n, Eq nl, Ord el) => EdgeListGraph (Gr n nl el) where
  labEdges (Gr g) = do
    (node, Context' _ _ s) <- HM.toList g
    (next, labels) <- HM.toList s
    label <- labels
    return $! LEdge (Edge node next) label

instance (Hashable n, Eq n, Eq nl, Ord el) => AdjacencyMatrix (Gr n nl el) where
  edgeExists (Gr g) (Edge src dst) = do
    (Context' _ _ s) <- HM.lookup src g
    (_, lbl) <- find ((==dst) . fst) (toAdj s)
    return lbl

instance (Hashable n, Eq n, Eq nl, Ord el) => MutableGraph (Gr n nl el) where
  (Context p ln@(LNode nid _) s) & (Gr g) =
    let c' = Context' (fromAdj p) ln (fromAdj s)
        !g1 = HM.insert nid c' g
        !g2 = addSucc g1 nid p
        !g3 = addPred g2 nid s
    in Gr g3

-- This graph has a canonical form so we can do fast comparisons
instance (Hashable n, Eq n, Eq nl, Ord el) => ComparableGraph (Gr n nl el) where
  graphEqual (Gr g1) (Gr g2) = g1 == g2

toAdj :: HashMap (Node (Gr n nl el)) [EdgeLabel (Gr n nl el)] -> Adj (Gr n nl el)
toAdj = concatMap expand . HM.toList
  where
    expand (n, ls) = map ((,) n) ls

fromAdj :: (Hashable n, Eq n, Ord el)
           => Adj (Gr n nl el)
           -> HashMap (Node (Gr n nl el)) [EdgeLabel (Gr n nl el)]
fromAdj = HM.fromListWith mergeLists . map (second return)

mergeLists :: (Ord a) => [a] -> [a] -> [a]
mergeLists [a] as =
  let newl = insert a as
  in length newl `seq` newl
mergeLists as [a] =
  let newl = insert a as
  in length newl `seq` newl
mergeLists xs ys =
  case length xs > length ys of
    True ->
      let newl = foldr insert xs ys
      in length newl `seq` newl
    False ->
      let newl = foldr insert ys xs
      in length newl `seq` newl

addSucc :: (Hashable n, Eq n, Ord el)
           => GraphRep n nl el
           -> Node (Gr n nl el)
           -> [(Node (Gr n nl el), el)]
           -> GraphRep n nl el
addSucc g _ []              = g
addSucc g v ((p, l) : rest) = addSucc g' v rest
    where
      !g' = HM.adjust f p g
      f (Context' ps l' ss) = Context' ps l' (HM.insertWith mergeLists v [l] ss)


addPred :: (Hashable n, Eq n, Ord el)
           => GraphRep n nl el
           -> Node (Gr n nl el)
           -> [(Node (Gr n nl el), el)]
           -> GraphRep n nl el
addPred g _ []              = g
addPred g v ((s, l) : rest) = addPred g' v rest
    where
      !g' = HM.adjust f s g
      f (Context' ps l' ss) = Context' (HM.insertWith mergeLists v [l] ps) l' ss


clearSucc :: (Hashable n, Eq n)
             => GraphRep n nl el
             -> Node (Gr n nl el)
             -> [Node (Gr n nl el)]
             -> GraphRep n nl el
clearSucc g _ []       = g
clearSucc g v (p:rest) = clearSucc g' v rest
    where
      !g' = HM.adjust f p g
      f (Context' ps l ss) = Context' ps l (HM.delete v ss)


clearPred :: (Hashable n, Eq n)
             => GraphRep n nl el
             -> Node (Gr n nl el)
             -> [Node (Gr n nl el)]
             -> GraphRep n nl el
clearPred g _ []       = g
clearPred g v (s:rest) = clearPred g' v rest
    where
      !g' = HM.adjust f s g
      f (Context' ps l ss) = Context' (HM.delete v ps) l ss
