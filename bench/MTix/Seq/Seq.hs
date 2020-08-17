{-# LANGUAGE DeriveGeneric #-}
module MTix.Seq.Seq where

import GHC.Generics (Generic)
import Control.DeepSeq
import System.FilePath

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Data.Set as Set

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Trace.Hpc.Tix
import Trace.Hpc.Mix
import Trace.Hpc.Util (HpcPos, fromHpcPos, writeFileUtf8, Hash)

------------------------------------------------------------------------------
-- A Tix variant with multiple tickers

-- Ticks count: (ticker, ticks)
type TicksCount = Map String Integer

showTicksCount :: TicksCount -> String
showTicksCount ticks =
  "Ticks per property: " <>
  concatMap (\(s,n) -> "&#10" <> s <> " : " <> show n <> " ticks") (Map.toList ticks)

tick :: String -> Integer -> TicksCount
tick = Map.singleton

ticksTotal :: TicksCount -> Integer
ticksTotal = sum . Map.elems

-- MTix files contaiting MTixModules
data MTix = MTix [MTixModule]
  deriving (Eq, Show, Read, Generic)

instance NFData MTix

-- MTix modules containing mdoule name, hash and ticks count
data MTixModule = MTixModule String Hash Int (Seq TicksCount)
  deriving (Eq, Show, Read, Generic)

instance NFData MTixModule

-- Hash is an opaque Word32 hidden behind a newtype. We cannot derive generic
-- for it because the constructor is hidden, so this is a nasty trick to define
-- a NFData instance that should work as long as (==) reduces the inner Word32s
-- to normal form.
instance NFData Hash where
  rnf x = rnf (x == x)

-- Projections
mtixModuleName :: MTixModule -> String
mtixModuleName (MTixModule nm _ _ _) = nm

mtixModuleHash :: MTixModule -> Hash
mtixModuleHash (MTixModule _ h  _ _) = h

mtixModuleTixs :: MTixModule -> Seq TicksCount
mtixModuleTixs (MTixModule  _ _ _ tixs) = tixs

----------------------------------------
-- Reading and merging MTixs

readMergeMTixs :: [FilePath] -> IO MTix
readMergeMTixs files = mergeMTixs <$> readMTixs files

readMTixs :: [FilePath] -> IO [MTix]
readMTixs = mapM readMTix

readMTix :: FilePath -> IO MTix
readMTix file = do
  mtix <- readTix file
  case mtix of
    Nothing -> error $ "error reading tix file from: " ++ file
    Just a -> return (tixToMTix (takeBaseName file) a)

tixToMTix :: String -> Tix -> MTix
tixToMTix prop (Tix xs) =
  MTix (toMTixModule <$> xs)
  where
    toMTixModule (TixModule n h i ticks) =
      MTixModule n h i (tick prop <$> Seq.fromList ticks)

----------------------------------------
-- Merging MTixs

mergeMTixs :: [MTix] -> MTix
mergeMTixs [] = error "mergeMTixs: empty input"
mergeMTixs xs = foldr1 mergeMTix xs

mergeMTix :: MTix -> MTix -> MTix
mergeMTix (MTix ts1) (MTix ts2) =
  MTix (zipWith mergeMTixModule ts1 ts2)

mergeMTixModule :: MTixModule -> MTixModule -> MTixModule
mergeMTixModule (MTixModule n1 h1 i1 tks1) (MTixModule n2 h2 i2 tks2)
  | n1 == n2 && h1 == h2 && i1 == i2 =
      MTixModule n1 h1 i1 (Seq.zipWith (<>) tks1 tks2)
  | otherwise =
      error $ "mergeMTixs: hash " <> show (h1, h2) <>
              " or module name "  <> show (n1, n2) <>
              " or list size "    <> show (i1, i2) <>
              " mismatch"

----------------------------------------
-- Projection over specific properties

projectMTix :: [String] -> MTix -> MTix
projectMTix props (MTix ts) = MTix (projectMTixModule props <$> ts)

projectMTixModule :: [String] -> MTixModule -> MTixModule
projectMTixModule props (MTixModule n h i tks) =
  MTixModule n h i (projectTicksCount props <$> tks)

projectTicksCount :: [String] -> TicksCount -> TicksCount
projectTicksCount props ticks = Map.restrictKeys ticks (Set.fromList props)

----------------------------------------
-- Expression coverage calculation

expCoverMTix :: FilePath -> MTix -> IO [(String, Double)]
expCoverMTix path (MTix mods) = mapM (expCoverMTixModule path) mods

expCoverMTixModule :: FilePath -> MTixModule -> IO (String, Double)
expCoverMTixModule path (MTixModule name hash size tix) = do
  Mix _ _ _ _ mix  <- readMix [path] (Left (takeFileName name)) -- this is a simplification!
  let (cov, tot) = foldr expCover (0,0) (Seq.zip (Seq.fromList mix) tix)
  let coverage = (fromIntegral cov * 100) / fromIntegral tot
  return (name, coverage)

expCover :: (MixEntry, TicksCount) -> (Integer, Integer) -> (Integer, Integer)
expCover ((_, ExpBox _), ticks) (cov, tot)
  | ticksTotal ticks > 0  = (cov+1, tot+1)
  | otherwise             = (cov,   tot+1)
expCover _ (cov, tot)     = (cov,   tot)
