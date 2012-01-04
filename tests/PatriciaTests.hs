{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main ( main ) where

import Control.Monad ( replicateM )
import Data.List ( foldl' )
import qualified Data.Set as S
import Test.Framework ( defaultMain, testGroup )
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.HUnit hiding ( Test, test )
import Test.QuickCheck

import Data.Graph.Interface
import Data.Graph.PatriciaTree
import Data.Graph.Algorithms.DFS

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "InternalProperties" iprops
            , testGroup "CondenseTests" ctests
            ]
    iprops = [ testProperty "allNodesInGraph" prop_nodesLenIsNumNodes
             , testProperty "matchAndMergeAreDual" prop_matchAndMergeAreDual
             , testProperty "gelemAndMatchAgree" prop_gelemAndMatchAgree
             ]
    ctests = [ testCase "scc1" test_scc1
             , testCase "condense1" test_condense1
             ]

type SG = HSGraph () ()

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

prop_matchAndMergeAreDual :: SG -> Property
prop_matchAndMergeAreDual g =
  gelem 2 g ==> case match 2 g of
    Nothing -> error "2 should be in g"
    Just (c, g') -> g `graphEqual` (c & g')

prop_gelemAndMatchAgree :: (Int, SG) -> Bool
prop_gelemAndMatchAgree (n, g) =
  maybe False (const True) (match n g) == gelem n g

cg1 :: SG
cg1 = mkGraph (map (\n -> LNode n ()) [1..5]) es
  where
    es = map (\(s,d) -> LEdge (Edge s d) ()) [(1,2), (2,1), (3,4), (4,3), (1,3), (5,4)]

test_scc1 :: Assertion
test_scc1 = do
  let comps = foldr (\ns s -> S.insert (S.fromList ns) s) S.empty (scc cg1)
      expected = S.fromList [ S.fromList [1,2]
                            , S.fromList [3,4]
                            , S.fromList [5]
                            ]
  assertEqual "test_scc1" expected comps

test_condense1 :: Assertion
test_condense1 = do
  let cg :: HSGraph [LNode SG] ()
      cg = condense cg1
      ordGroups = topsort' cg
      comps = foldl' (\acc ns -> S.fromList (map unlabelNode ns) : acc) [] ordGroups
      expected = [S.fromList [4,3], S.fromList [5], S.fromList [1,2]]
  assertEqual "test_condense1" expected comps
