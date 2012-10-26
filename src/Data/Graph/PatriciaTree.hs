{-# LANGUAGE TypeFamilies, BangPatterns #-}
module Data.Graph.PatriciaTree ( Gr ) where

import Control.Arrow
import Control.DeepSeq
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.List ( foldl', find )
import Data.Maybe ( fromMaybe )
import Data.Graph.Interface

-- import Debug.Trace
-- debug = flip trace

data Ctx a b = Ctx !(IntMap b) a !(IntMap b)
newtype Gr a b = Gr { graphRepr :: IntMap (Ctx a b) }


instance Graph (Gr n e) where
  type VertexLabel (Gr n e) = n
  type EdgeLabel (Gr n e) = e

  empty = Gr IM.empty
  isEmpty = IM.null . graphRepr

instance InspectableGraph (Gr n e) where
  context (Gr g) (V v) = do
    Ctx p l s <- IM.lookup v g
    return $! Context (toAdj p) (V v) l (toAdj s)

toAdj :: IntMap b -> [(Vertex, b)]
toAdj = map (first V) . IM.toList

{-

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
-}

instance IncidenceGraph (Gr n e) where
  out g v = fromMaybe [] $ do
    Context _ src _ s <- context g v
    return $ map (toEdge src) s
    where
      toEdge src (dst, lbl) = Edge src dst lbl

instance BidirectionalGraph (Gr n e) where
  inn g v = fromMaybe [] $ do
    Context p dst _ _ <- context g v
    return $ map (toEdge dst) p
    where
      toEdge dst (src, lbl) = Edge src dst lbl

instance AdjacencyGraph (Gr n e) where
  foldSuc f seed (Gr gr) (V v) = fromMaybe seed $ do
    Ctx _ _ s <- IM.lookup v gr
    return $ IM.foldrWithKey' f' seed s
    where
      f' k = f (V k)

instance BidirectionalAdjacencyGraph (Gr n e) where
  foldPre f seed (Gr gr) (V v) = fromMaybe seed $ do
    Ctx p _ _ <- IM.lookup v gr
    return $ IM.foldrWithKey' f' seed p
    where
      f' k = f (V k)

{-

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

-- | Remove @v@ from the predecessor list of each node in @ns@
clearPred :: GraphRep a b -> Int -> [Int] -> GraphRep a b
clearPred g _ []       = g
clearPred g v ns =
  foldl' (flip (IM.adjust f)) g ns
  where
    f (Context' ps l ss) = Context' (IM.delete v ps) l ss
-}