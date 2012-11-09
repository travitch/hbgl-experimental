{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main ( main ) where

import Control.Monad ( replicateM )
import Data.List ( foldl' )
import Data.Maybe ( fromMaybe )
import qualified Data.Set as S
import Test.Framework ( defaultMain, testGroup )
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.HUnit hiding ( Test, test )
import Test.QuickCheck

import Data.Graph.Interface
import Data.Graph.MutableDigraph
import Data.Graph.Algorithms.DFS
import Data.Graph.Algorithms.Condense

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "InternalProperties" iprops
            , testGroup "CondenseTests" ctests
            ]
    iprops = [ testProperty "allNodesInGraph" prop_nodesLenIsNumNodes
--             , testProperty "matchAndMergeAreDual" prop_matchAndMergeAreDual
             , testProperty "gelemAndMatchAgree" prop_gelemAndMatchAgree
             , testProperty "prop_matchRemovesNodeRefs" prop_matchRemovesNodeRefs
             , testProperty "prop_insNodeWorks" prop_insNodeWorks
             , testProperty "prop_insEdgeUpdatesSuc" prop_insEdgeUpdatesSuc
             , testProperty "prop_insEdgeUpdatesPre" prop_insEdgeUpdatesPre
             , testProperty "prop_insEdgeUpdatesSelfSuc" prop_insEdgeUpdatesSelfSuc
             , testProperty "prop_insEdgeUpdatesSelfPre" prop_insEdgeUpdatesSelfPre
             ]
    ctests = [ testCase "scc1" test_scc1
             , testCase "condense1" test_condense1
             ]

type SG = SparseDigraph () ()

instance Arbitrary SG where
  arbitrary = arbitraryGraph

instance Show SG where
  show g = concat [ "Verts: "
                  , show (vertices g)
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
  let ns = zip [0..sz] (repeat ()) -- map (\n -> (n, ())) [0..sz]
  nEdges <- choose (2, 2 * sz)
  srcs <- replicateM nEdges (choose (0, sz))
  dsts <- replicateM nEdges (choose (0, sz))
  let es = zipWith (\s d -> (Edge s d ())) srcs dsts
  return (mkGraph ns es)

prop_nodesLenIsNumNodes :: SG -> Bool
prop_nodesLenIsNumNodes g = numVertices g == length (vertices g)

-- prop_matchAndMergeAreDual :: (NodeId, SG) -> Property
-- prop_matchAndMergeAreDual (NID n, g) =
--   gelem n g ==> case match n g of
--     Nothing -> error (show n ++ " should be in g")
--     Just (c, g') -> g `graphEqual` (c & g')

prop_gelemAndMatchAgree :: (NodeId, SG) -> Bool
prop_gelemAndMatchAgree (NID n, g) =
  maybe False (const True) (match n g) == gelem n g

prop_matchRemovesNodeRefs :: (NodeId, SG) -> Bool
prop_matchRemovesNodeRefs (NID n, g) =
  case match n g of
    Nothing -> True
    Just (_, g') ->
      all (\(Edge s d _) -> s /= n && d /= n) (edges g')

-- insertEdge can return Nothing if the edge already exists; that is a
-- passing test here and in the UpdatesPre test
prop_insEdgeUpdatesSuc :: (NodeId, NodeId, SG) -> Bool
prop_insEdgeUpdatesSuc (NID s, NID d, g) = fromMaybe True $ do
  g' <- insertEdge s d () g
  return $ d `elem` suc g' s

prop_insEdgeUpdatesPre :: (NodeId, NodeId, SG) -> Bool
prop_insEdgeUpdatesPre (NID s, NID d, g) = fromMaybe True $ do
  g' <- insertEdge s d () g
  return $ s `elem` pre g' d

prop_insEdgeUpdatesSelfSuc :: (NodeId, SG) -> Bool
prop_insEdgeUpdatesSelfSuc (NID d, g) = fromMaybe True $ do
  g' <- insertEdge d d () g
  return $ d `elem` suc g' d

prop_insEdgeUpdatesSelfPre :: (NodeId, SG) -> Bool
prop_insEdgeUpdatesSelfPre (NID d, g) = fromMaybe True $ do
  g' <- insertEdge d d () g
  return $ d `elem` pre g' d

prop_insNodeWorks :: (NodeId, SG) -> Bool
prop_insNodeWorks (NID n, g) =
  let g' = insertVertex (n+100) () g
  in gelem (n + 100) g'

cg1 :: SG
cg1 = mkGraph (map (\n -> (n, ())) [1..5]) es
  where
    es = map (\(s,d) -> Edge s d ()) [(1,2), (2,1), (3,4), (4,3), (1,3), (5,4)]

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
  let cg :: SparseDigraph [(Vertex, VertexLabel SG)] () --  [LNode SG] ()
      cg = condense cg1
      ordGroups = topsort' cg
      comps = foldl' (\acc ns -> S.fromList (map fst ns) : acc) [] ordGroups
      expected = [S.fromList [4,3], S.fromList [5], S.fromList [1,2]]
  assertEqual "test_condense1" expected comps
