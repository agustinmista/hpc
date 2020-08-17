{-# LANGUAGE BangPatterns #-}
module MTix.Par.Coverage2 where

import GHC.Conc
import System.FilePath
import Data.List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Data.Set as Set

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Control.Parallel
import Control.Parallel.Strategies

import Trace.Hpc.Tix
import Trace.Hpc.Mix
import MTix.Par.Types

----------------------------------------
-- Parallel expression coverage
-- * parallel over tix modules
-- * parallel over tix entries (in chunks)

expCoverMTix :: [(String, Mix)] -> MTix -> [(String, Double)]
expCoverMTix mixs (MTix mods) = do
  parMap rdeepseq (expCoverMTixModule mixs) mods

expCoverMTixModule :: [(String, Mix)] -> MTixModule -> (String, Double)
expCoverMTixModule mixs (MTixModule name hash size tix) = do
  (name, (fromIntegral cov * 100) / fromIntegral tot)
  where
    (cov, tot) = mapReduceChunks rdeepseq 200 mapper reducer pairs
    reducer = foldr (\(c1, t1) (c2, t2) -> (c1+c2, t1+t2)) (0,0)
    mapper = foldr expCover (0,0)
    pairs = Seq.zip (Seq.fromList mix) tix
    Just (Mix _ _ _ _ mix)  = lookup (takeFileName name) mixs
    mixName (Mix m _ _ _ _) = takeFileName m

expCover :: (MixEntry, TicksCount) -> (Integer, Integer) -> (Integer, Integer)
expCover ((_, ExpBox _), ticks) (cov, tot)
  | ticksTotal ticks > 0  = (cov+1, tot+1)
  | otherwise             = (cov,   tot+1)
expCover _ (cov, tot)     = (cov,   tot)

----------------------------------------
-- Parallel combinators

mapReduceChunks :: Strategy b -> Int -> (Seq a -> b) -> (Seq b -> c) -> Seq a -> c
mapReduceChunks s n mapper reducer xs =
  reducer (fmap mapper chunks `using` parTraversable s)
  where
    chunks = Seq.chunksOf n xs
