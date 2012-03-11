{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Data.Graph.Algorithms.Matching.Dominators (
  dom,
  iDom
  ) where

import Data.Graph.Interface
import Data.Graph.Algorithms.Matching.DFS

import Data.Tree ( Tree(..) )
import qualified Data.Tree as T
import Data.Array
import Data.Map ( Map )
import qualified Data.Map as M

iDom g root =
  map (\(a, b) -> (toNode ! a, toNode ! b)) (assocs result)
  where
    (result, toNode, _) = idomWork g root

dom :: forall gr . (DecomposableGraph gr,
        BidirectionalAdjacencyGraph gr,
        VertexListGraph gr,
        Ord (Node gr))
       => gr -> Node gr -> [(Node gr, [Node gr])]
dom g root =
  -- [(toNode ! i, dom' ! i) | i <- range (bounds dom')]
  x' ++ [(n, nodes') | n <- rest]
  where
--    t :: (IDom, ToNode (Node gr), FromNode (Node gr))
    t@(idoms, toNode, fromNode) = idomWork g root
--    dom' :: Array Node' [Node gr]
    dom' = getDom toNode idoms
--    nodes' :: [Node gr]
    nodes' = nodes g
    rest = M.keys (M.filter (-1 ==) fromNode)

    x = assocs dom'
    x' = map (\(ix, d) -> (toNode ! ix, d)) x


type Node' = Int
type IDom = Array Node' Node'
type Preds = Array Node' [Node']
type ToNode n = Array Node' n
type FromNode n = Map n Node'

idomWork :: (DecomposableGraph gr,
             BidirectionalAdjacencyGraph gr,
             VertexListGraph gr,
             Ord (Node gr))
            => gr -> Node gr -> (IDom, ToNode (Node gr), FromNode (Node gr))
idomWork g root =
  case null trees of
    True -> error "Dominators.idomWork: root not in graph"
    False -> (doms, toNode, fromNode)
  where
    trees@(~[tree]) = dff [root] g
    (s, ntree) = numberTree 0 tree
    idom0 = array (1, s-1) (tail $ treeEdges (-1) ntree)
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

getDom :: (Graph gr) => ToNode (Node gr) -> IDom -> Array Node' [Node gr]
getDom toNode idom =
  let res = array (0, snd (bounds idom)) ((0, [toNode ! 0]) :
                     [(i, toNode ! i : res ! (idom ! i)) | i <- range (bounds idom)])
  in res

numberTree :: Node' -> Tree a -> (Node', Tree Node')
numberTree n (Node _ ts) =
  (n', Node n ts')
  where
    (n', ts') = numberForest (n + 1) ts

numberForest :: Node' -> [Tree a] -> (Node', [Tree Node'])
numberForest n [] = (n, [])
numberForest n (t:ts) =
  (n'', t' : ts')
  where
    (n', t') = numberTree n t
    (n'', ts') = numberForest n' ts

treeEdges :: a -> Tree a -> [(a, a)]
treeEdges a (Node b ts) = (b, a) : concatMap (treeEdges b) ts

fixEq :: (Eq a) => (a -> a) -> a -> a
fixEq f v | v' == v = v
          | otherwise = fixEq f v'
  where
    v' = f v