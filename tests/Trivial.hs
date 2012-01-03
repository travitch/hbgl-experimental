module Main ( main ) where

import Data.Graph.Interface
import Data.Graph.PatriciaTree


type TestGraph1 = HSGraph Char String

g0 :: TestGraph1
g0 = mkGraph ns es
  where
    ns = [ LNode 1 'a'
         , LNode 2 'b'
         , LNode 3 'c'
         , LNode 4 'd'
         ]
    es = [ LEdge (Edge 1 2) "e1"
         , LEdge (Edge 3 2) "e2"
         , LEdge (Edge 4 2) "e3"
         , LEdge (Edge 1 2) "e1"
         , LEdge (Edge 1 2) "e4"
         ]

type TestGraph2 = LGraph Char String

g1 :: TestGraph2
g1 = mkGraph ns es
  where
    ns = [ LNode 1 'a'
         , LNode 2 'b'
         , LNode 3 'c'
         , LNode 4 'd'
         ]
    es = [ LEdge (Edge 1 2) "e1"
         , LEdge (Edge 3 2) "e2"
         , LEdge (Edge 4 2) "e3"
         , LEdge (Edge 1 2) "e1"
         , LEdge (Edge 1 2) "e4"
         ]

main :: IO ()
main = do
  putStrLn "g0"
  print (pre g0 2)
  print (suc g0 1)
  putStrLn "g1"
  print (pre g1 2)
  print (suc g1 1)