{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Data.Graph.Algorithms.Marking.Dominators (
  dom,
  iDom
  ) where

import Data.Graph.Interface
import Data.Graph.Algorithms.Marking.DFS

import Data.Tree ( Tree(..) )
import qualified Data.Tree as T
import Data.Array
import Data.List ( foldl' )
import Data.Map ( Map )
import qualified Data.Map as M

iDom :: (BidirectionalAdjacencyGraph gr,
         VertexListGraph gr,
         Ord (Node gr)) =>
        gr -> Node gr -> [(Node gr, Node gr)]
iDom g root =
  map (\(a, b) -> (toNode ! a, toNode ! b)) (assocs result)
  where
    (result, toNode, _) = idomWork g root

dom :: (BidirectionalAdjacencyGraph gr,
        VertexListGraph gr,
        Ord (Node gr))
       => gr -> Node gr -> [(Node gr, [Node gr])]
dom g root =
  [(toNode ! i, dom' ! i) | i <- range (bounds dom')] ++
    [(n, nodes') | n <- rest]
  where
    (idoms, toNode, fromNode) = idomWork g root
    -- This is inlined (it was not in fgl) to work around a bit of a
    -- problem with injectivity and associated types.
    dom' = array (0, snd (bounds idoms)) ((0, [toNode ! 0]) :
                     [(i, toNode ! i : dom' ! (idoms ! i)) | i <- range (bounds idoms)])

    nodes' = nodes g
    rest = M.keys (M.filter (-1 ==) fromNode)


type Node' = Int
type IDom = Array Node' Node'
type Preds = Array Node' [Node']
type ToNode gr = Array Node' (Node gr)
type FromNode gr = Map (Node gr) Node'

idomWork :: (BidirectionalAdjacencyGraph gr,
             VertexListGraph gr,
             Ord (Node gr))
            => gr -> Node gr -> (IDom, ToNode gr, FromNode gr)
idomWork g root =
  case null trees of
    True -> error "Dominators.idomWork: root not in graph"
    False -> (doms, toNode, fromNode)
  where
    trees@(~[tree]) = dff [root] g
    (s, ntree) = numberTree 0 tree
    idom0 = array (1, s-1) (treeEdges ntree)
    fromNode = M.unionWith const (M.fromList (zip (T.flatten tree) (T.flatten ntree))) (M.fromList (zip (nodes g) (repeat (-1))))
    toNode = array (0, s-1) (zip (T.flatten ntree) (T.flatten tree))
    preds = array (1, s-1) [(i, filter (/= -1) (map (fromNode M.!) (pre g (toNode ! i)))) | i <- [1..s-1]]
    doms = fixEq (refineIDom preds) idom0

refineIDom :: Preds -> IDom -> IDom
refineIDom preds idom = fmap (foldl1 (intersect idom)) preds

intersect :: IDom -> Node' -> Node' -> Node'
intersect idom a b =
  case a `compare` b of
    LT -> intersect idom a (idom ! b)
    EQ -> a
    GT -> intersect idom (idom ! a) b

numberTree :: Node' -> Tree a -> (Node', Tree Node')
numberTree !n !(Node _ ts) =
  (n', Node n ts')
  where
    (n', ts') = numberForest (n + 1) ts

numberForest :: Node' -> [Tree a] -> (Node', [Tree Node'])
numberForest !n [] = (n, [])
numberForest !n (t:ts) =
  (n'', t' : ts')
  where
    (n', t') = numberTree n t
    (n'', ts') = numberForest n' ts

treeEdges :: Tree a -> [(a, a)]
treeEdges = go []
  where
    go acc (Node a ts) =
      let es = map (\t -> (rootLabel t, a)) ts
      in foldl' go (es ++ acc) ts

fixEq :: (Eq a) => (a -> a) -> a -> a
fixEq f v | v' == v = v
          | otherwise = fixEq f v'
  where
    v' = f v