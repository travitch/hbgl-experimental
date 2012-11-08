{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main ( main ) where

import Control.Monad ( replicateM )
import Data.List ( sort )
import Data.Tree ( flatten )
import Test.Framework ( defaultMain, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck

import Data.Graph.Interface
import qualified Data.Graph.Inductive as FGL
import qualified Data.Graph.ImmutableDigraph as MG
import qualified Data.Graph.Algorithms.DFS as DFS

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


type FGLGraph = FGL.Gr () ()
type TGraph = MG.DenseImmutableDigraph () ()

data GraphPair = GP TGraph FGLGraph

instance Arbitrary GraphPair where
  arbitrary = arbitraryGraphPair

instance Show GraphPair where
  show (GP g1 g2) =
    concat [ "TGraphNodes: "
           , show (vertices g1)
           , "\n"
           , show (edges g1)
           , "\nCGraphNodes: "
           , show (FGL.nodes g2)
           , "\n"
           , show (FGL.edges g2)
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
  let ns1 = zip [0..sz] (repeat ()) -- map (\n -> LNode n ()) [0..sz]
      ns2 = map (\n -> (n, ())) [0..sz]
  nEdges <- choose (2, 2 * sz)
  srcs <- replicateM nEdges (choose (0, sz))
  dsts <- replicateM nEdges (choose (0, sz))
  let es1 = zipWith (\s d -> Edge s d ()) srcs dsts
      es2 = zipWith (\s d -> (s, d, ())) srcs dsts
  return $! GP (mkGraph ns1 es1) (FGL.mkGraph ns2 es2)

prop_dfsSame :: (NodeId, GraphPair) -> Bool
prop_dfsSame (NID nid, GP tg cg) =
  sort (DFS.dfs [nid] tg) == sort (FGL.dfs [nid] cg)

prop_dfsPrimeSame :: GraphPair -> Bool
prop_dfsPrimeSame (GP tg cg) =
  sort (DFS.dfs' tg) == sort (FGL.dfs' cg)

prop_dffSame :: (NodeId, GraphPair) -> Bool
prop_dffSame (NID nid, GP tg cg) =
  let f1 = sort $ map (sort . flatten) $ DFS.dff [nid] tg
      f2 = sort $ map (sort . flatten) $ FGL.dff [nid] cg
  in f1 == f2 `debug` printf "f1: %s\nf2: %s\n\n" (show f1) (show f2)
--  sort (map flatten (Match.dff [nid] tg)) == sort (map flatten (FGL.dff [nid] cg))

prop_dffPrimeSame :: GraphPair -> Bool
prop_dffPrimeSame (GP tg cg) =
  sort (map (sort . flatten) (DFS.dff' tg)) == sort (map (sort . flatten) (FGL.dff' cg))

prop_equalNComponents :: GraphPair -> Bool
prop_equalNComponents (GP tg cg) =
  DFS.noComponents tg == FGL.noComponents cg

prop_equalComponents :: GraphPair -> Bool
prop_equalComponents (GP tg cg) =
  let c1 = sort (map sort (DFS.components tg))
      c2 = sort (map sort (FGL.components cg))
  in c1 == c2 -- `debug` printf "C1: %s\nC2: %s\n" (show c1) (show c2)
