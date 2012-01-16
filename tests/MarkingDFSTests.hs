{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main ( main ) where

import Control.Monad ( replicateM )
import Data.List ( sort )
import Data.Tree ( flatten )
import Test.Framework ( defaultMain, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck

import Data.Graph.Interface
import qualified Data.Graph.Compact as C
import qualified Data.Graph.PatriciaTree as P
import qualified Data.Graph.Algorithms.Marking.DFS as Mark
import qualified Data.Graph.Algorithms.Matching.DFS as Match

import Text.Printf
import Debug.Trace
debug = flip trace

-- | This suite only tests a few of the DFS functions because they all
-- use the same underlying search mechanisms.  If these few work, I am
-- reasonably confident that the others work, too.
--
-- This works by comparing the results of the matching and marking
-- implementations, which should return the same results for all
-- cases.  The PatriciaTree implementation uses the List-based link
-- storage, since the compact adjacency list also uses lists.  This
-- should avoid edge uniqueness differences.
main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "EquivalentResults" props
            ]
    props = [ testProperty "prop_dfsSame" prop_dfsSame
            , testProperty "prop_dfsPrimeSame" prop_dfsPrimeSame
            , testProperty "prop_dffSame" prop_dffSame
            , testProperty "prop_dffPrimeSame" prop_dffPrimeSame
            , testProperty "prop_equalNComponents" prop_equalNComponents
            , testProperty "prop_equalComponents" prop_equalComponents
            ]


type CGraph = C.Gr Int () ()
type TGraph = P.LGraph () ()

data GraphPair = GP TGraph CGraph

instance Arbitrary GraphPair where
  arbitrary = arbitraryGraphPair

instance Show GraphPair where
  show (GP g1 g2) =
    concat [ "TGraphNodes: "
           , show (nodes g1)
           , "\n"
           , show (edges g1)
           , "\nCGraphNodes: "
           , show (nodes g2)
           , "\n"
           , show (edges g2)
           ]

arbitraryGraphPair :: Gen GraphPair
arbitraryGraphPair = sized mkGraphPair

newtype NodeId = NID Int
instance Arbitrary NodeId where
  arbitrary = sized mkNodeId
    where
      mkNodeId n = do
        i <- choose (0, n)
        return (NID i)
instance Show NodeId where
  show (NID i) = show i


mkGraphPair :: Int -> Gen GraphPair
mkGraphPair sz = do
  let ns1 = map (\n -> LNode n ()) [0..sz]
      ns2 = map (\n -> LNode n ()) [0..sz]
  nEdges <- choose (2, 2 * sz)
  srcs <- replicateM nEdges (choose (0, sz))
  dsts <- replicateM nEdges (choose (0, sz))
  let es1 = zipWith (\s d -> LEdge (Edge s d) ()) srcs dsts
      es2 = zipWith (\s d -> LEdge (Edge s d) ()) srcs dsts
  return $! GP (mkGraph ns1 es1) (mkGraph ns2 es2)

prop_dfsSame :: (NodeId, GraphPair) -> Bool
prop_dfsSame (NID nid, GP tg cg) =
  sort (Match.dfs [nid] tg) == sort (Mark.dfs [nid] cg)

prop_dfsPrimeSame :: GraphPair -> Bool
prop_dfsPrimeSame (GP tg cg) =
  sort (Match.dfs' tg) == sort (Mark.dfs' cg)

prop_dffSame :: (NodeId, GraphPair) -> Bool
prop_dffSame (NID nid, GP tg cg) =
  sort (map flatten (Match.dff [nid] tg)) == sort (map flatten (Mark.dff [nid] cg))

prop_dffPrimeSame :: GraphPair -> Bool
prop_dffPrimeSame (GP tg cg) =
  sort (map flatten (Match.dff' tg)) == sort (map flatten (Mark.dff' cg))

prop_equalNComponents :: GraphPair -> Bool
prop_equalNComponents (GP tg cg) =
  Match.noComponents tg == Mark.noComponents cg

prop_equalComponents :: GraphPair -> Bool
prop_equalComponents (GP tg cg) =
  let c1 = sort (map sort (Match.components tg))
      c2 = sort (map sort (Mark.components cg))
  in c1 == c2 `debug` printf "C1: %s\nC2: %s\n" (show c1) (show c2)
