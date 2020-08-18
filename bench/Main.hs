{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Conc
import GHC.Generics

import System.Directory
import System.FilePath

import Control.Monad
import Control.DeepSeq
import Control.Concurrent
import Criterion.Main
import Criterion.Types

import Trace.Hpc.Tix
import Trace.Hpc.Mix
import Trace.Hpc.Util

-- Sequential versions
import qualified MTix.Seq.List        as SeqList
import qualified MTix.Seq.Map         as SeqMap
import qualified MTix.Seq.Seq         as SeqSeq

-- Parallel versions
import MTix.Par.Types
import qualified MTix.Par.Read        as ParRead
import qualified MTix.Par.Merge1      as ParMerge1
import qualified MTix.Par.Merge2      as ParMerge2
import qualified MTix.Par.Merge3      as ParMerge3
import qualified MTix.Par.Project1    as ParProject1
import qualified MTix.Par.Project2    as ParProject2
import qualified MTix.Par.Coverage1   as ParCoverage1
import qualified MTix.Par.Coverage2   as ParCoverage2

----------------------------------------
-- Some generic noise

instance NFData Mix where
  rnf x = rnf (show x)

----------------------------------------
-- FilePath utilities

resultsDir :: FilePath
resultsDir = "bench" </> "results"

dataDir :: FilePath
dataDir = "bench" </> "data"

tixDir :: FilePath -> FilePath
tixDir x = dataDir </> x </> "tix"

mixDir :: FilePath -> FilePath
mixDir x = dataDir </> x </> "mix"

listTixFiles :: FilePath -> IO [FilePath]
listTixFiles f = do
  filenames <- listDirectory (tixDir f)
  return ((tixDir f </>) <$> filenames)

readMixs :: FilePath -> [String] -> IO [(String, Mix)]
readMixs dir = mapM $ \modName -> do
  mix <- readMix [dir] (Left (takeFileName modName))
  return (takeFileName modName, mix)

----------------------------------------
-- Benchmark drivers

benchReadMTixs :: NFData mtix => String -> ([FilePath] -> IO [mtix]) -> [FilePath] -> [Benchmark]
benchReadMTixs name readOp inputs =
  [ bench (name <> "/" <> show n) (nfAppIO readOp (take n inputs))
  | n <- 1 : [ round ((len / 9) * i) | i <- [1..9] ] ]
  where len = fromIntegral (length inputs)

benchReadMTixs' :: NFData mtix => String -> ([FilePath] -> IO [mtix]) -> [FilePath] -> [Benchmark]
benchReadMTixs' name readOp inputs =
  [ bench (name <> "/" <> show (length inputs)) (nfAppIO readOp inputs) ]

benchMergeMTixs :: NFData mtix => String -> ([mtix] -> mtix) -> [mtix] -> [Benchmark]
benchMergeMTixs name mergeOp inputs =
  [ bench (name <> "/" <> show n) (nf mergeOp (take n inputs))
  | n <- 1 : [ round ((len / 9) * i) | i <- [1..9] ] ]
  where len = fromIntegral (length inputs)

benchMergeMTixs' :: NFData mtix => String -> ([mtix] -> mtix) -> [mtix] -> [Benchmark]
benchMergeMTixs' name mergeOp inputs =
  [ bench (name <> "/" <> show (length inputs)) (nf mergeOp inputs) ]

benchProjectMTix :: NFData mtix => String -> ([String] -> mtix -> mtix) -> [FilePath] -> mtix -> [Benchmark]
benchProjectMTix name projOp props mtix =
  [ bench (name <> "/" <> show n) (nf (flip projOp mtix) (takeBaseName <$> take n props))
  | n <- 1 : [ round ((len / 9) * i) | i <- [1..9] ] ]
  where len = fromIntegral (length props)

benchProjectMTix' :: NFData mtix => String -> ([String] -> mtix -> mtix) -> [FilePath] -> mtix -> [Benchmark]
benchProjectMTix' name projOp props mtix =
  [ bench (name <> "/" <> show n) (nf (flip projOp mtix) (takeBaseName <$> take n props)) ]
  where n = length props `div` 2

benchExpCoverMTix :: NFData mtix => String -> ([(String, Mix)] -> mtix -> [(String, Double)]) -> [(String, Mix)] -> mtix -> [Benchmark]
benchExpCoverMTix name coverOp mixs mtix =
  [ bench name (nf (coverOp mixs) mtix) ]

----------------------------------------
-- Benchmarks

sequentialBenchmarks :: IO ()
sequentialBenchmarks = do
  putStrLn "******* Benchmarking sequential operations"

  cores     <- getNumCapabilities

  xmonad    <- listTixFiles "xmonad"
  primitive <- listTixFiles "primitive"
  bst       <- listTixFiles "bst"

  -- :: [MTix]
  !xmonad_seq_list    <- force <$> SeqList.readMTixs xmonad
  !primitive_seq_list <- force <$> SeqList.readMTixs primitive
  !bst_seq_list       <- force <$> SeqList.readMTixs bst

  !xmonad_seq_map    <- force <$> SeqMap.readMTixs xmonad
  !primitive_seq_map <- force <$> SeqMap.readMTixs primitive
  !bst_seq_map       <- force <$> SeqMap.readMTixs bst

  !xmonad_seq_seq    <- force <$> SeqSeq.readMTixs xmonad
  !primitive_seq_seq <- force <$> SeqSeq.readMTixs primitive
  !bst_seq_seq       <- force <$> SeqSeq.readMTixs bst

  -- :: MTix
  !xmonad_seq_list_mtix    <- force <$> SeqList.readMergeMTixs xmonad
  !primitive_seq_list_mtix <- force <$> SeqList.readMergeMTixs primitive
  !bst_seq_list_mtix       <- force <$> SeqList.readMergeMTixs bst

  !xmonad_seq_map_mtix     <- force <$> SeqMap.readMergeMTixs  xmonad
  !primitive_seq_map_mtix  <- force <$> SeqMap.readMergeMTixs  primitive
  !bst_seq_map_mtix        <- force <$> SeqMap.readMergeMTixs  bst

  !xmonad_seq_seq_mtix     <- force <$> SeqSeq.readMergeMTixs  xmonad
  !primitive_seq_seq_mtix  <- force <$> SeqSeq.readMergeMTixs  primitive
  !bst_seq_seq_mtix        <- force <$> SeqSeq.readMergeMTixs  bst

  -- :: [Mix]
  !xmonad_mixs    <- force <$> readMixs (mixDir "xmonad")    (SeqList.mtixModules xmonad_seq_list_mtix)
  !primitive_mixs <- force <$> readMixs (mixDir "primitive") (SeqList.mtixModules primitive_seq_list_mtix)
  !bst_mixs       <- force <$> readMixs (mixDir "bst")       (SeqList.mtixModules bst_seq_list_mtix)

  ----------------------------------------
  putStrLn "******* Benchmarking readMTixs"
  let csv = resultsDir </> "seq" </> "readMTixs" <.> show cores <.> "csv"
  defaultMainWith defaultConfig { csvFile = Just csv } $ concat $
    [ benchReadMTixs "SeqList/xmonad"    SeqList.readMTixs xmonad
    , benchReadMTixs "SeqList/primitive" SeqList.readMTixs primitive
    , benchReadMTixs "SeqList/bst"       SeqList.readMTixs bst
    , benchReadMTixs "SeqMap/xmonad"     SeqMap.readMTixs  xmonad
    , benchReadMTixs "SeqMap/primitive"  SeqMap.readMTixs  primitive
    , benchReadMTixs "SeqMap/bst"        SeqMap.readMTixs  bst
    , benchReadMTixs "SeqSeq/xmonad"     SeqSeq.readMTixs  xmonad
    , benchReadMTixs "SeqSeq/primitive"  SeqSeq.readMTixs  primitive
    , benchReadMTixs "SeqSeq/bst"        SeqSeq.readMTixs  bst
    ]

  ----------------------------------------
  putStrLn "******* Benchmarking mergeMTixs"
  let csv = resultsDir </> "seq" </> "mergeMTixs" <.> show cores <.> "csv"
  defaultMainWith defaultConfig { csvFile = Just csv } $ concat $
    [ benchMergeMTixs "SeqList/xmonad"    SeqList.mergeMTixs xmonad_seq_list
    , benchMergeMTixs "SeqList/primitive" SeqList.mergeMTixs primitive_seq_list
    , benchMergeMTixs "SeqList/bst"       SeqList.mergeMTixs bst_seq_list
    , benchMergeMTixs "SeqMap/xmonad"     SeqMap.mergeMTixs  xmonad_seq_map
    , benchMergeMTixs "SeqMap/primitive"  SeqMap.mergeMTixs  primitive_seq_map
    , benchMergeMTixs "SeqMap/bst"        SeqMap.mergeMTixs  bst_seq_map
    , benchMergeMTixs "SeqSeq/xmonad"     SeqSeq.mergeMTixs  xmonad_seq_seq
    , benchMergeMTixs "SeqSeq/primitive"  SeqSeq.mergeMTixs  primitive_seq_seq
    , benchMergeMTixs "SeqSeq/bst"        SeqSeq.mergeMTixs  bst_seq_seq
    ]

  ----------------------------------------
  putStrLn "******* Benchmarking projectMTixs"
  let csv = resultsDir </> "seq" </> "projectMTix" <.> show cores <.> "csv"
  defaultMainWith defaultConfig { csvFile = Just csv } $ concat $
    [ benchProjectMTix "SeqList/xmonad"    SeqList.projectMTix xmonad    xmonad_seq_list_mtix
    , benchProjectMTix "SeqList/primitive" SeqList.projectMTix primitive primitive_seq_list_mtix
    , benchProjectMTix "SeqList/bst"       SeqList.projectMTix bst       bst_seq_list_mtix
    , benchProjectMTix "SeqMap/xmonad"     SeqMap.projectMTix  xmonad    xmonad_seq_map_mtix
    , benchProjectMTix "SeqMap/primitive"  SeqMap.projectMTix  primitive primitive_seq_map_mtix
    , benchProjectMTix "SeqMap/bst"        SeqMap.projectMTix  bst       bst_seq_map_mtix
    , benchProjectMTix "SeqSeq/xmonad"     SeqSeq.projectMTix  xmonad    xmonad_seq_seq_mtix
    , benchProjectMTix "SeqSeq/primitive"  SeqSeq.projectMTix  primitive primitive_seq_seq_mtix
    , benchProjectMTix "SeqSeq/bst"        SeqSeq.projectMTix  bst       bst_seq_seq_mtix
    ]

  ----------------------------------------
  putStrLn "******* Benchmarking coverageMTix"
  let csv = resultsDir </> "seq" </> "coverageMTix" <.> show cores <.> "csv"
  defaultMainWith defaultConfig { csvFile = Just csv } $ concat $

    [ benchExpCoverMTix "SeqList/xmonad"    SeqList.expCoverMTix xmonad_mixs    xmonad_seq_list_mtix
    , benchExpCoverMTix "SeqList/primitive" SeqList.expCoverMTix primitive_mixs primitive_seq_list_mtix
    , benchExpCoverMTix "SeqList/bst"       SeqList.expCoverMTix bst_mixs       bst_seq_list_mtix
    , benchExpCoverMTix "SeqMap/xmonad"     SeqMap.expCoverMTix  xmonad_mixs    xmonad_seq_map_mtix
    , benchExpCoverMTix "SeqMap/primitive"  SeqMap.expCoverMTix  primitive_mixs primitive_seq_map_mtix
    , benchExpCoverMTix "SeqMap/bst"        SeqMap.expCoverMTix  bst_mixs       bst_seq_map_mtix
    , benchExpCoverMTix "SeqSeq/xmonad"     SeqSeq.expCoverMTix  xmonad_mixs    xmonad_seq_seq_mtix
    , benchExpCoverMTix "SeqSeq/primitive"  SeqSeq.expCoverMTix  primitive_mixs primitive_seq_seq_mtix
    , benchExpCoverMTix "SeqSeq/bst"        SeqSeq.expCoverMTix  bst_mixs       bst_seq_seq_mtix
    ]


parallelBenchmarks :: IO ()
parallelBenchmarks = do
  putStrLn "******* Benchmarking parallel operations"

  cores     <- getNumCapabilities

  xmonad    <- listTixFiles "xmonad"
  primitive <- listTixFiles "primitive"
  bst       <- listTixFiles "bst"

  !xmonad_mtixs    <- force <$> ParRead.readMTixs xmonad
  !primitive_mtixs <- force <$> ParRead.readMTixs primitive
  !bst_mtixs       <- force <$> ParRead.readMTixs bst

  !xmonad_mtix    <- return (force (ParMerge2.mergeMTixs xmonad_mtixs))
  !primitive_mtix <- return (force (ParMerge2.mergeMTixs primitive_mtixs))
  !bst_mtix       <- return (force (ParMerge2.mergeMTixs bst_mtixs))

  !xmonad_mixs    <- force <$> readMixs (mixDir "xmonad")    (mtixModules xmonad_mtix)
  !primitive_mixs <- force <$> readMixs (mixDir "primitive") (mtixModules primitive_mtix)
  !bst_mixs       <- force <$> readMixs (mixDir "bst")       (mtixModules bst_mtix)

  ----------------------------------------

  -- putStrLn "******* Benchmarking readMTixs"
  -- let csv = resultsDir </> "par" </> "readMTixs" <.> show cores <.> "csv"
  -- defaultMainWith defaultConfig { csvFile = Just csv } $ concat $
  --   [ benchReadMTixs' "ParRead/xmonad"    ParRead.readMTixs xmonad
  --   , benchReadMTixs' "ParRead/primitive" ParRead.readMTixs primitive
  --   , benchReadMTixs' "ParRead/bst"       ParRead.readMTixs bst
  --   ]
 
  -- withDelaysIO ParRead.readMTixs primitive

  ----------------------------------------
  
  -- putStrLn "******* Benchmarking mergeMTixs"
  -- let csv = resultsDir </> "par" </> "mergeMTixs" <.> show cores <.> "csv"
  -- defaultMainWith defaultConfig { csvFile = Just csv } $ concat $
  --   [ benchMergeMTixs' "Merge1/xmonad"    ParMerge1.mergeMTixs xmonad_mtixs
  --   , benchMergeMTixs' "Merge1/primitive" ParMerge1.mergeMTixs primitive_mtixs
  --   , benchMergeMTixs' "Merge1/bst"       ParMerge1.mergeMTixs bst_mtixs
  --   , benchMergeMTixs' "Merge2/xmonad"    ParMerge2.mergeMTixs xmonad_mtixs
  --   , benchMergeMTixs' "Merge2/primitive" ParMerge2.mergeMTixs primitive_mtixs
  --   , benchMergeMTixs' "Merge2/bst"       ParMerge2.mergeMTixs bst_mtixs
  --   , benchMergeMTixs' "Merge3/xmonad"    ParMerge3.mergeMTixs xmonad_mtixs
  --   , benchMergeMTixs' "Merge3/primitive" ParMerge3.mergeMTixs primitive_mtixs
  --   , benchMergeMTixs' "Merge3/bst"       ParMerge3.mergeMTixs bst_mtixs
  --   ]
  
  -- withDelays ParMerge1.mergeMTixs primitive_mtixs
  withDelays ParMerge2.mergeMTixs primitive_mtixs
  -- withDelays ParMerge3.mergeMTixs primitive_mtixs

  ----------------------------------------
  
  -- putStrLn "******* Benchmarking projectMTix"
  -- let csv = resultsDir </> "par" </> "projectMTix" <.> show cores <.> "csv"
  -- defaultMainWith defaultConfig { csvFile = Just csv } $ concat $
  --   [ benchProjectMTix' "ParProject1/xmonad"    ParProject1.projectMTix xmonad    xmonad_mtix
  --   , benchProjectMTix' "ParProject1/primitive" ParProject1.projectMTix primitive primitive_mtix
  --   , benchProjectMTix' "ParProject1/bst"       ParProject1.projectMTix bst       bst_mtix
  --   , benchProjectMTix' "ParProject2/xmonad"    ParProject2.projectMTix xmonad    xmonad_mtix
  --   , benchProjectMTix' "ParProject2/primitive" ParProject2.projectMTix primitive primitive_mtix
  --   , benchProjectMTix' "ParProject2/bst"       ParProject2.projectMTix bst       bst_mtix
  --   ]

  -- withDelays (ParProject1.projectMTix (take (length primitive `div` 2) primitive)) primitive_mtix
  -- withDelays (ParProject2.projectMTix (take (length primitive `div` 2) primitive)) primitive_mtix

  ----------------------------------------
  
  -- putStrLn "******* Benchmarking expCoverMTix"
  -- let csv = resultsDir </> "par" </> "expCoverMTix" <.> show cores <.> "csv"
  -- defaultMainWith defaultConfig { csvFile = Just csv } $ concat $
  --   [ benchExpCoverMTix "ParCoverage1/xmonad"    ParCoverage1.expCoverMTix xmonad_mixs    xmonad_mtix
  --   , benchExpCoverMTix "ParCoverage1/primitive" ParCoverage1.expCoverMTix primitive_mixs primitive_mtix
  --   , benchExpCoverMTix "ParCoverage1/bst"       ParCoverage1.expCoverMTix bst_mixs       bst_mtix
  --   , benchExpCoverMTix "ParCoverage2/xmonad"    ParCoverage2.expCoverMTix xmonad_mixs    xmonad_mtix
  --   , benchExpCoverMTix "ParCoverage2/primitive" ParCoverage2.expCoverMTix primitive_mixs primitive_mtix
  --   , benchExpCoverMTix "ParCoverage2/bst"       ParCoverage2.expCoverMTix bst_mixs       bst_mtix
  --   ]

  -- withDelays (ParCoverage1.expCoverMTix primitive_mixs) primitive_mtix
  -- withDelays (ParCoverage2.expCoverMTix primitive_mixs) primitive_mtix

----------------------------------------
-- Run an operation with some delays to make it easy to spot its boundaries in threadscope

withDelays :: (NFData a, NFData b) => (a -> b) -> a -> IO ()
withDelays op input = do
  !input' <- return (force input)
  threadDelay 1000000
  !res <- return (force (op input))
  threadDelay 1000000
  print (rnf res)

withDelaysIO :: (NFData a, NFData b) => (a -> IO b) -> a -> IO ()
withDelaysIO op input = do
  !input' <- return (force input)
  threadDelay 1000000
  !res <- force <$> op input
  threadDelay 1000000
  print (rnf res)

----------------------------------------
-- Entry point

main :: IO ()
main = do
  putStrLn ("Running with " <> show numCapabilities <> " HECs")
  when (numCapabilities == 1) sequentialBenchmarks
  parallelBenchmarks
  putStrLn "Finished!"
