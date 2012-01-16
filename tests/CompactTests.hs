{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main ( main ) where

import Control.Monad ( replicateM )
import Data.List ( foldl', sort )
import qualified Data.Set as S
import Test.Framework ( defaultMain, testGroup )
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.HUnit hiding ( Test, test )
import Test.QuickCheck

import Data.Graph.Interface
import Data.Graph.Compact
-- import Data.Graph.Algorithms.DFS

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "InternalProperties" iprops
            , testGroup "ExpectedTests" expectedTests
            ]
    iprops = [ testProperty "allNodesInGraph" prop_nodesLenIsNumNodes
             , testProperty "prop_nodeIsSourceOfOutEdge" prop_nodeIsSourceOfOutEdge
             ]
    expectedTests = [ testCase "expectedOut1" test_expectedOut1
                    , testCase "test_expectedIn1" test_expectedIn1
                    , testCase "test_expectedIn2" test_expectedIn2
                    ]

type SG = Gr Int () ()

instance Arbitrary SG where
  arbitrary = arbitraryGraph

instance Show SG where
  show g = concat [ "Nodes: "
                  , show (nodes g)
                  , "\n"
                  , show (edges g)
                  ]

arbitraryGraph :: Gen SG
arbitraryGraph = sized mkSG

newtype NodeId = NID Int
instance Arbitrary NodeId where
  arbitrary = sized mkNodeId
    where
      mkNodeId n = do
        i <- choose (0, n)
        return (NID i)
instance Show NodeId where
  show (NID i) = show i


mkSG :: Int -> Gen SG
mkSG sz = do
  let ns = map (\n -> LNode n ()) [0..sz]
  nEdges <- choose (2, 2 * sz)
  srcs <- replicateM nEdges (choose (0, sz))
  dsts <- replicateM nEdges (choose (0, sz))
  let es = zipWith (\s d -> LEdge (Edge s d) ()) srcs dsts
  return (mkGraph ns es)

prop_nodesLenIsNumNodes :: SG -> Bool
prop_nodesLenIsNumNodes g = noNodes g == length (nodes g)

prop_nodeIsSourceOfOutEdge :: (NodeId, SG) -> Bool
prop_nodeIsSourceOfOutEdge (NID nid, g) =
  all isSrcOfEdge (out g nid)
  where
    isSrcOfEdge (Edge s _) = s == nid

prop_nodeIsDestOfInEdge :: (NodeId, SG) -> Bool
prop_nodeIsDestOfInEdge (NID nid, g) =
  all isDestOfEdge (inn g nid)
  where
    isDestOfEdge (Edge _ d) = d == nid

eg1 :: SG
eg1 = mkGraph (map (\n -> LNode n ()) [1..5]) es
  where
    es = map mkEdge [(1,2), (1,3), (1,4), (4,1), (4,5)]
    mkEdge (s,d) = LEdge (Edge s d) ()

test_expectedOut1 :: Assertion
test_expectedOut1 = do
  let expected = sort $ [Edge 1 2, Edge 1 3, Edge 1 4]
      computed = sort $ out eg1 1
  assertEqual "test_expectedOut1" expected computed

test_expectedIn1 :: Assertion
test_expectedIn1 = do
  let expected = [Edge 4 1]
      computed = inn eg1 1
  assertEqual "test_expectedIn1" expected computed

test_expectedIn2 :: Assertion
test_expectedIn2 = do
  let expected = [Edge 1 4]
      computed = inn eg1 4
  assertEqual "test_expectedIn2" expected computed
