-- | Shortest path algorithms
module Data.Graph.Algorithms.Matching.SP (
  sp,
  spTree,
  dijkstra
  ) where

import Data.Heap ( Heap, Entry(..) )
import qualified Data.Heap as H
import Data.Monoid

import Data.Graph.Interface

-- Path = [Node]
-- LRTree = [[LNode]]
-- RTree = [[Node]]

expand :: (EdgeLabel g -> Double)
          -> Double
          -> [(Node g, Double)]
          -> Context g
          -> [Heap (Entry Double [(Node g, Double)])]
expand toWeight w p (Context _ _ s) =
  map (\(v, l) -> H.singleton (Entry (toWeight l+w) ((v, toWeight l+w) : p))) s

findP :: (Eq a) => a -> [[(a, t)]] -> [(a, t)]
findP _ [] = []
findP v ([] : ps) = findP v ps
findP v (p@((w,_):_) : ps)
  | v == w = p
  | otherwise = findP v ps

getLPath :: (Eq a) => a -> [[(a, t)]] -> [(a, t)]
getLPath v = reverse . findP v

getLPathNodes :: (Eq a) => a -> [[(a, b)]] -> [a]
getLPathNodes v = map fst . getLPath v

sp :: (DecomposableGraph g)
      => (EdgeLabel g -> Double)
      -> Node g
      -> Node g
      -> g
      -> [Node g]
sp toWeight s t = getLPathNodes t . spTree toWeight s

spTree :: (DecomposableGraph g)
          => (EdgeLabel g -> Double)
          -> Node g
          -> g
          -> [[(Node g, Double)]]
spTree toWeight v = dijkstra toWeight (H.singleton (Entry 0 [(v, 0)]))

dijkstra :: (DecomposableGraph g)
            => (EdgeLabel g -> Double)
            -> Heap (Entry Double [(Node g, Double)])
            -> g
            -> [[(Node g, Double)]]
dijkstra _ h g | H.null h || isEmpty g = []
dijkstra toWeight h g =
  case match v g of
    Nothing -> dijkstra toWeight h' g
    Just (c, g') ->
      p : dijkstra toWeight (foldr H.union mempty (h' : expand toWeight w p c)) g'
  where
    Just (Entry _ p, h') = H.viewMin h
    ((v, w):_) = p
