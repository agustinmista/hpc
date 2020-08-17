{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Conc

import System.Directory
import System.FilePath

import Control.DeepSeq
import Control.Concurrent
import Criterion.Main
import Criterion.Types

-- Sequential versions
import qualified MTix.Seq.List        as SeqList
import qualified MTix.Seq.Map         as SeqMap
import qualified MTix.Seq.Seq         as SeqSeq

-- Parallel versions
-- import qualified MTixParRead       as ParRead
-- import qualified MTixParMerge1     as ParMerge1
-- import qualified MTixParMerge2     as ParMerge2
-- import qualified MTixParReadMerge1 as ParReadMerge1
-- import qualified MTixParProject1   as ParProject1
-- import qualified MTixParProject2   as ParProject2

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
  basenames <- listDirectory (tixDir f)
  return ((tixDir f </>) <$> basenames)

----------------------------------------
-- Benchmark drivers

benchReadMTixs :: NFData mtix => String -> ([FilePath] -> IO [mtix]) -> [FilePath] -> [Benchmark]
benchReadMTixs name readOp inputs =
  [ bench (name <> "/" <> show n) (nfAppIO readOp (take n inputs))
  | n <- 1 : [ round ((len / 9) * i) | i <- [1..9] ] ]
  where len = fromIntegral (length inputs)

benchMergeMTixs :: NFData mtix => String -> ([mtix] -> mtix) -> [mtix] -> [Benchmark]
benchMergeMTixs name mergeOp inputs =
  [ bench (name <> "/" <> show n) (nf mergeOp (take n inputs))
  | n <- 1 : [ round ((len / 9) * i) | i <- [1..9] ] ]
  where len = fromIntegral (length inputs)

benchProjectMTix :: NFData mtix => String -> ([String] -> mtix -> mtix) -> [FilePath] -> mtix -> [Benchmark]
benchProjectMTix name projOp props mtix =
  [ bench (name <> "/" <> show n) (nf (flip projOp mtix) (takeBaseName <$> take n props))
  | n <- 1 : [ round ((len / 9) * i) | i <- [1..9] ] ]
  where len = fromIntegral (length props)

benchCoverageMTix :: NFData mtix => String -> (mtix -> IO [(String, Double)]) -> mtix -> [Benchmark]
benchCoverageMTix name coverOp mtix =
  [ bench name (nfAppIO coverOp mtix) ]


----------------------------------------
-- Benchmarks

sequentialBenchmarks :: IO ()
sequentialBenchmarks = do

  cores     <- getNumCapabilities

  xmonad    <- listTixFiles "xmonad"
  primitive <- listTixFiles "primitive"
  bst       <- listTixFiles "bst"

  -- Some precomputed values needed for benchmarks

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

  ----------------------------------------
  -- readMTixs

  putStrLn "******* Benchmarking readMTixs"
  let csv = resultsDir </> "readMTixs" <.> show cores <.> "csv"
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
  let csv = resultsDir </> "mergeMTixs" <.> show cores <.> "csv"
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
  let csv = resultsDir </> "projectMTix" <.> show cores <.> "csv"
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
  let csv = resultsDir </> "coverageMTix" <.> show cores <.> "csv"
  defaultMainWith defaultConfig { csvFile = Just csv } $ concat $

    [ benchCoverageMTix "SeqList/xmonad"    (SeqList.expCoverMTix (mixDir "xmonad"))    xmonad_seq_list_mtix
    , benchCoverageMTix "SeqList/primitive" (SeqList.expCoverMTix (mixDir "primitive")) primitive_seq_list_mtix
    , benchCoverageMTix "SeqList/bst"       (SeqList.expCoverMTix (mixDir "bst"))       bst_seq_list_mtix

    , benchCoverageMTix "SeqMap/xmonad"     (SeqMap.expCoverMTix  (mixDir "xmonad"))    xmonad_seq_map_mtix
    , benchCoverageMTix "SeqMap/primitive"  (SeqMap.expCoverMTix  (mixDir "primitive")) primitive_seq_map_mtix
    , benchCoverageMTix "SeqMap/bst"        (SeqMap.expCoverMTix  (mixDir "bst"))       bst_seq_map_mtix

    , benchCoverageMTix "SeqSeq/xmonad"     (SeqSeq.expCoverMTix  (mixDir "xmonad"))    xmonad_seq_seq_mtix
    , benchCoverageMTix "SeqSeq/primitive"  (SeqSeq.expCoverMTix  (mixDir "primitive")) primitive_seq_seq_mtix
    , benchCoverageMTix "SeqSeq/bst"        (SeqSeq.expCoverMTix  (mixDir "bst"))       bst_seq_seq_mtix
    ]


----------------------------------------
-- Entry point

main :: IO ()
main = do
  sequentialBenchmarks
  putStrLn "Finished!"

  ----------------------------------------

  -- threadDelay 1000000
  -- print (xmonad_par1 `deepseq` length (show (ParMerge1.mergeMTixs xmonad_par1)))
  -- threadDelay 1000000
  -- print (length (show (SeqList.mergeMTixs xmonad_seq_list)))
  -- threadDelay 1000000
