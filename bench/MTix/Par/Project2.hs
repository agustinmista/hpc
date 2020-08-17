module MTix.Par.Project2 where

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
-- Parallel projection over specific properties
-- * parallel over tix modules
-- * parallel over tix entries (in chunks)

projectMTix :: [String] -> MTix -> MTix
projectMTix props (MTix ts) =
  MTix (parMap rdeepseq (projectMTixModule props) ts)

projectMTixModule :: [String] -> MTixModule -> MTixModule
projectMTixModule props (MTixModule n h i tks) =
  MTixModule n h i (parSeqMapChunk rdeepseq 50 (projectTicksCount props) tks)

projectTicksCount :: [String] -> TicksCount -> TicksCount
projectTicksCount props ticks = Map.restrictKeys ticks (Set.fromList props)

----------------------------------------
-- Parallel combinators

parSeqMapChunk :: Strategy (Seq a) -> Int -> (a -> a) -> Seq a -> Seq a
parSeqMapChunk s n f xs =
  concatSeqs (fmap (fmap f) chunks `using` parTraversable s)
  where
    concatSeqs = foldr (Seq.><) Seq.empty
    chunks = Seq.chunksOf n xs
