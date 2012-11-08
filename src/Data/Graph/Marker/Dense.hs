module Data.Graph.Marker.Dense ( DenseMarker ) where

import Data.Bits
import Data.Vector.Unboxed.Mutable ( STVector )
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Word ( Word64 )

import Data.Graph.Interface

data DenseMarker s = DenseMarker (STVector s Word64)

bitsPerWord :: Int
bitsPerWord = bitSize (undefined :: Word64)

instance MarksVertices DenseMarker where
  newMarker nElems = do
    let nWords = ceiling ((fromIntegral nElems / fromIntegral bitsPerWord) :: Double)
    v <- V.replicate nWords 0
    return $ DenseMarker v

  markVertex (DenseMarker v) vid = do
    let ix = floor ((fromIntegral vid / fromIntegral bitsPerWord) :: Double)
        bitPos = vid `mod` bitsPerWord
    oldWord <- V.read v ix
    let newWord = setBit oldWord bitPos
    V.write v ix newWord

  isVertexMarked (DenseMarker v) vid = do
    let ix = floor ((fromIntegral vid / fromIntegral bitsPerWord) :: Double)
        bitPos = vid `mod` bitsPerWord
    w <- V.read v ix
    return $ testBit w bitPos