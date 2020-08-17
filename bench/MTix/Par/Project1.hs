module MTix.Par.Project1 where

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
-- * sequential over tix entries

projectMTix :: [String] -> MTix -> MTix
projectMTix props (MTix ts) =
  MTix (parMap rdeepseq (projectMTixModule props) ts)

projectMTixModule :: [String] -> MTixModule -> MTixModule
projectMTixModule props (MTixModule n h i tks) =
  MTixModule n h i (fmap (projectTicksCount props) tks)

projectTicksCount :: [String] -> TicksCount -> TicksCount
projectTicksCount props ticks = Map.restrictKeys ticks (Set.fromList props)
