{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Conc

import System.Directory
import System.FilePath

import Control.DeepSeq
import Control.Concurrent
import Criterion.Main
import Criterion.Types

import qualified MTix              as Base
import qualified MTixParRead       as ParRead
import qualified MTixParMerge1     as ParMerge1
import qualified MTixParMerge2     as ParMerge2
import qualified MTixParReadMerge1 as ParReadMerge1
import qualified MTixParProject1   as ParProject1
import qualified MTixParProject2   as ParProject2

dataDir :: FilePath
dataDir = "bench" </> "data"

resultsDir :: FilePath
resultsDir = "bench" </> "results"

listDataFiles :: FilePath -> IO [FilePath]
listDataFiles f = do
  basenames <- listDirectory (dataDir </> f)
  return ((\b -> dataDir </> f </> b) <$> basenames)

benchReadMTixs :: NFData mtix => String -> ([FilePath] -> IO [mtix]) -> [FilePath] -> [Benchmark]
benchReadMTixs name readOp inputs =
  [ bench (name <> "/" <> show n) (nfAppIO readOp (take n inputs))
  | n <- 1 : [ round ((len / 9) * i) | i <- [1..9] ]
  ]
  where
    len = fromIntegral (length inputs)

benchMergeMTixs :: NFData mtix => String -> ([mtix] -> mtix) -> [mtix] -> [Benchmark]
benchMergeMTixs name mergeOp inputs =
  [ bench (name <> "/" <> show n) (nf mergeOp (take n inputs))
  | n <- 1 : [ round ((len / 9) * i) | i <- [1..9] ]
  ]
  where
    len = fromIntegral (length inputs)

benchProjectMTix :: NFData mtix => String -> ([String] -> mtix -> mtix) -> [FilePath] -> mtix -> [Benchmark]
benchProjectMTix name op props mtix =
  [ bench (name <> "/" <> show n) (nf (flip op mtix) (takeBaseName <$> take n props))
  | n <- 1 : [ round ((len / 9) * i) | i <- [1..9] ]
  ]
  where
    len = fromIntegral (length props)

main :: IO ()
main = do
  cores     <- getNumCapabilities
  xmonad    <- listDataFiles "xmonad"
  primitive <- listDataFiles "primitive"
  bst       <- listDataFiles "bst"

  -- ----------------------------------------
  -- -- Sequential vs parallel read

  -- putStrLn "******* Benchmarking readMTixs"
  -- let csv = resultsDir </> "readMTixs" <.> show cores <.> "csv"
  -- defaultMainWith defaultConfig { csvFile = Just csv } $ concat $

  --   -- Sequential read
  --   [ benchReadMTixs "Base/xmonad"    Base.readMTixs xmonad
  --   , benchReadMTixs "Base/primitive" Base.readMTixs primitive
  --   , benchReadMTixs "Base/bst"       Base.readMTixs bst
  --   ] <>

  --   -- Parallel read
  --   [ benchReadMTixs "ParRead/xmonad"    ParRead.readMTixs xmonad
  --   , benchReadMTixs "ParRead/primitive" ParRead.readMTixs primitive
  --   , benchReadMTixs "ParRead/bst"       ParRead.readMTixs bst
  --   ]

  ----------------------------------------
  -- Sequential vs parallel merge

  -- !xmonad_base    <- force <$> Base.readMTixs xmonad
  -- !primitive_base <- force <$> Base.readMTixs primitive
  -- !bst_base       <- force <$> Base.readMTixs bst

  !xmonad_par1    <- force <$> ParMerge1.readMTixs xmonad
  -- !primitive_par1 <- force <$> ParMerge1.readMTixs primitive
  -- !bst_par1       <- force <$> ParMerge1.readMTixs bst

  -- !xmonad_par2    <- force <$> ParMerge2.readMTixs xmonad
  -- !primitive_par2 <- force <$> ParMerge2.readMTixs primitive
  -- !bst_par2       <- force <$> ParMerge2.readMTixs bst

  -- putStrLn "******* Benchmarking mergeMTixs"
  -- let csv = resultsDir </> "mergeMTixs" <.> show cores <.> "csv"
  -- defaultMainWith defaultConfig { csvFile = Just csv } $ concat $

  --   -- Sequential merge
  --   [ benchMergeMTixs "Base/xmonad"    Base.mergeMTixs xmonad_base
  --   -- , benchMergeMTixs "Base/primitive" Base.mergeMTixs primitive_base
  --   -- , benchMergeMTixs "Base/bst"       Base.mergeMTixs bst_base
  --   ] <>

  --   -- Parallel merge 1
  --   [ benchMergeMTixs "ParMerge1/xmonad"    ParMerge1.mergeMTixs xmonad_par1
  --   -- , benchMergeMTixs "ParMerge1/primitive" ParMerge1.mergeMTixs primitive_par1
  --   -- , benchMergeMTixs "ParMerge1/bst"       ParMerge1.mergeMTixs bst_par1
  --   ] <>

  --   -- Parallel merge 2
  --   [ benchMergeMTixs "ParMerge2/xmonad"    ParMerge2.mergeMTixs xmonad_par2
  --   -- , benchMergeMTixs "ParMerge2/primitive" ParMerge2.mergeMTixs primitive_par2
  --   -- , benchMergeMTixs "ParMerge2/bst"       ParMerge2.mergeMTixs bst_par2
  --   ]

  -- ----------------------------------------
  -- -- Sequential vs parallel projection

  -- !xmonad_base_mtix    <- force <$> Base.readMergeMTixs xmonad
  -- !primitive_base_mtix <- force <$> Base.readMergeMTixs primitive
  -- !bst_base_mtix       <- force <$> Base.readMergeMTixs bst

  -- !xmonad_par_mtix    <- force <$> ParProject1.readMergeMTixs xmonad
  -- !primitive_par_mtix <- force <$> ParProject1.readMergeMTixs primitive
  -- !bst_par_mtix       <- force <$> ParProject1.readMergeMTixs bst

  -- !xmonad_par2_mtix    <- force <$> ParProject2.readMergeMTixs xmonad
  -- !primitive_par2_mtix <- force <$> ParProject2.readMergeMTixs primitive
  -- !bst_par2_mtix       <- force <$> ParProject2.readMergeMTixs bst

  -- putStrLn "******* Benchmarking projectMTixs"
  -- let csv = resultsDir </> "projectMTix" <.> show cores <.> "csv"
  -- defaultMainWith defaultConfig { csvFile = Just csv } $ concat $

  --   -- Sequential project
  --   [ benchProjectMTix "Base/xmonad"    Base.projectMTix xmonad    xmonad_base_mtix
  --   , benchProjectMTix "Base/primitive" Base.projectMTix primitive primitive_base_mtix
  --   , benchProjectMTix "Base/bst"       Base.projectMTix bst       bst_base_mtix
  --   ] <>

  --   -- Parallel project 1
  --   [ benchProjectMTix "ParProject1/xmonad"    ParProject1.projectMTix xmonad    xmonad_par_mtix
  --   , benchProjectMTix "ParProject1/primitive" ParProject1.projectMTix primitive primitive_par_mtix
  --   , benchProjectMTix "ParProject1/bst"       ParProject1.projectMTix bst       bst_par_mtix
  --   ] <>

  --   -- Parallel project 2
  --   [ benchProjectMTix "ParProject2/xmonad"    ParProject2.projectMTix xmonad    xmonad_par2_mtix
  --   , benchProjectMTix "ParProject2/primitive" ParProject2.projectMTix primitive primitive_par2_mtix
  --   , benchProjectMTix "ParProject2/bst"       ParProject2.projectMTix bst       bst_par2_mtix
  --   ]

  threadDelay 1000000
  -- let !mtix = Base.mergeMTixs xmonad_base
  let !mtix = ParMerge1.mergeMTixs xmonad_par1
  threadDelay 1000000
  print (rnf mtix)

  putStrLn "Finished!"
