module MTix.Par.Coverage1 where

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

expCoverMTix :: [(String, Mix)] -> MTix -> [(String, Double)]
expCoverMTix mixs (MTix mods) = do
  parMap rdeepseq (expCoverMTixModule mixs) mods

expCoverMTixModule :: [(String, Mix)] -> MTixModule -> (String, Double)
expCoverMTixModule mixs (MTixModule name hash size tix) = do
  (name, (fromIntegral cov * 100) / fromIntegral tot)
  where
    (cov, tot) = foldr expCover (0,0) (Seq.zip (Seq.fromList mix) tix)
    mix = case lookup (takeFileName name) mixs of
      Just (Mix _ _ _ _ mix) -> mix
      Nothing -> error ("expCoverMTixModule: " <> show (takeFileName name) <> " not in " <> show (fst <$> mixs))
    mixHash (Mix _ _ h _ _) = h

expCover :: (MixEntry, TicksCount) -> (Integer, Integer) -> (Integer, Integer)
expCover ((_, ExpBox _), ticks) (cov, tot)
  | ticksTotal ticks > 0  = (cov+1, tot+1)
  | otherwise             = (cov,   tot+1)
expCover _ (cov, tot)     = (cov,   tot)
