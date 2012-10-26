{-# LANGUAGE TypeFamilies, BangPatterns #-}
module Data.Graph.PatriciaTree ( Gr ) where

import Control.Arrow
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Data.Maybe ( fromMaybe )
import Data.Monoid

import Data.Graph.Interface

data Ctx a b = Ctx !(IntMap b) a !(IntMap b)
data Gr a b = Gr { graphRepr :: IntMap (Ctx a b)
                 , graphIdSrc :: !Int
                 }

instance Graph (Gr n e) where
  type VertexLabel (Gr n e) = n
  type EdgeLabel (Gr n e) = e

  empty = Gr IM.empty 0
  isEmpty = IM.null . graphRepr

instance InspectableGraph (Gr n e) where
  context (Gr g _) (V v) = do
    Ctx p l s <- IM.lookup v g
    return $! Context (toAdj p) (V v) l (toAdj s)

toAdj :: IntMap b -> [(Vertex, b)]
toAdj = map (first V) . IM.toList

instance DecomposableGraph (Gr n e) where
  match v g = do
    c <- context g v
    return (c, removeVertex v g)

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
  foldSuc f seed (Gr gr _) (V v) = fromMaybe seed $ do
    Ctx _ _ s <- IM.lookup v gr
    return $ IM.foldrWithKey' f' seed s
    where
      f' k = f (V k)

instance BidirectionalAdjacencyGraph (Gr n e) where
  foldPre f seed (Gr gr _) (V v) = fromMaybe seed $ do
    Ctx p _ _ <- IM.lookup v gr
    return $ IM.foldrWithKey' f' seed p
    where
      f' k = f (V k)

instance VertexListGraph (Gr n e) where
  labeledVertices = IM.foldrWithKey' toLabV [] . graphRepr
    where
      toLabV vid (Ctx _ l _) acc = (V vid, l) : acc
  vertices = map V . IM.keys . graphRepr
  numVertices = IM.size . graphRepr

instance EdgeListGraph (Gr n e) where
  edges = IM.foldrWithKey' toEdges [] . graphRepr
    where
      toEdges src (Ctx _ _ ss) acc =
        IM.foldrWithKey' (toEdge src) acc ss
      toEdge src dst lbl acc = Edge (V src) (V dst) lbl : acc

instance AdjacencyMatrix (Gr n e) where
  edgeExists (Gr g _) (V src) (V dst) =
    fromMaybe [] $ do
      Ctx _ _ ss <- IM.lookup src g
      lbl <- IM.lookup dst ss
      return [lbl]

instance MutableGraph (Gr n e) where
  insertVertex lbl (Gr g vid) =
    (Gr (IM.insert vid (Ctx mempty lbl mempty) g) (vid + 1), V vid)

  insertEdge s@(V src) d@(V dst) lbl (Gr g vid) = do
    Ctx sps slbl sss <- IM.lookup src g
    Ctx dps dlbl dss <- IM.lookup dst g
    case IM.member dst sss of
      True -> Nothing
      False -> do
        let !sss' = IM.insert dst lbl sss
            !dps' = IM.insert src lbl dps
            !g1 = IM.insert src (Ctx sps slbl sss') g
            !g2 = IM.insert dst (Ctx dps' dlbl dss) g1
        return (Gr g2 vid, Edge s d lbl)

  removeVertex v@(V vid) g =
    let Gr g1 idSrc = clearVertex v g
        g2 = IM.delete vid g1
    in Gr g2 idSrc

  clearVertex (V v) g@(Gr g0 idSrc) =
    fromMaybe g $ do
      Ctx ps lbl ss <- IM.lookup v g0
      let !g1 = foldr (IM.adjust (removeSucc v)) g0 (IM.keys ps)
          !g2 = foldr (IM.adjust (removePred v)) g1 (IM.keys ss)
          !g3 = IM.insert v (Ctx mempty lbl mempty) g2
      return (Gr g3 idSrc)
    where
      removeSucc vid (Ctx ps lbl ss) = Ctx ps lbl (IM.delete vid ss)
      removePred vid (Ctx ps lbl ss) = Ctx (IM.delete vid ps) lbl ss

  removeEdge (Edge (V src) (V dst) lbl) g@(Gr gr idsrc) =
    fromMaybe g $ do
      Ctx ps slbl ss <- IM.lookup src gr
      Ctx pps dlbl pss <- IM.lookup dst gr
      elbl <- IM.lookup dst ss
      case elbl == lbl of
        False -> Nothing
        True -> do
          let !g1 = IM.insert src (Ctx ps slbl (IM.delete dst ss)) gr
              !g2 = IM.insert dst (Ctx (IM.delete src pps) dlbl pss) g1
          return (Gr g2 idsrc)

  removeEdges (V src) (V dst) g@(Gr gr idsrc) =
    fromMaybe g $ do
      Ctx ps slbl ss <- IM.lookup src gr
      Ctx pps dlbl pss <- IM.lookup dst gr
      let !g1 = IM.insert src (Ctx ps slbl (IM.delete dst ss)) gr
          !g2 = IM.insert dst (Ctx (IM.delete src pps) dlbl pss) g1
      return (Gr g2 idsrc)
