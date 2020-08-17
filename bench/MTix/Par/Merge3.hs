module MTix.Par.Merge3 where

import GHC.Conc
import System.FilePath

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Data.Set as Set

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Control.Parallel
import Control.Parallel.Strategies

import Trace.Hpc.Tix
import MTix.Par.Types

----------------------------------------
-- Parallel merge
-- * granularity is per tix entry (in chunks)
-- * sequential folding

mergeMTixs :: [MTix] -> MTix
mergeMTixs [] = error "mergeMTixs: empty input"
mergeMTixs xs = foldr1 mergeMTix xs

mergeMTix :: MTix -> MTix -> MTix
mergeMTix (MTix ts1) (MTix ts2) =
  MTix (parZipWith rdeepseq mergeMTixModule ts1 ts2)

mergeMTixModule :: MTixModule -> MTixModule -> MTixModule
mergeMTixModule (MTixModule n1 h1 i1 tks1) (MTixModule n2 h2 i2 tks2)
  | n1 == n2 && h1 == h2 && i1 == i2 =
      let n = max 500 (i1 `div` numCapabilities)
      in  MTixModule n1 h1 i1 (parSeqZipWithChunk rdeepseq n (<>) tks1 tks2)
  | otherwise =
      error $ "mergeMTixs: hash " <> show (h1, h2) <>
              " or module name "  <> show (n1, n2) <>
              " or list size "    <> show (i1, i2) <>
              " mismatch"

----------------------------------------
-- Parallel combinators

parZipWith :: Strategy a -> (a -> a -> a) -> [a] -> [a] -> [a]
parZipWith s f xs ys = zipWith f xs ys `using` parList s

parSeqZipWithChunk :: Strategy (Seq a) -> Int -> (a -> a -> a) -> Seq a -> Seq a -> Seq a
parSeqZipWithChunk s n f xs ys =
  concatSeqs (fmap zipChunk chunks `using` parTraversable s)
  where
    concatSeqs = foldr (Seq.><) Seq.empty
    zipChunk = fmap (\(x, y) -> f x y)
    chunks = Seq.chunksOf n (Seq.zip xs ys)
