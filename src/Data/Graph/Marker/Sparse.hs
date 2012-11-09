module Data.Graph.Marker.Sparse ( SparseMarker ) where

import Data.HashTable.ST.Basic ( HashTable )
import qualified Data.HashTable.ST.Basic as HT

import Data.Graph.Interface

data SparseMarker s = SparseMarker (HashTable s Vertex ())

instance MarksVertices SparseMarker where
  newMarker g = do
    let nElems = numVertices g
    t <- HT.newSized (2 * nElems)
    return $ SparseMarker t

  markVertex (SparseMarker t) v = HT.insert t v ()

  isVertexMarked (SparseMarker t) v = do
    token <- HT.lookup t v
    return $ maybe False (const True) token
