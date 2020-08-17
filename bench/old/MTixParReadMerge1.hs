{-# LANGUAGE DeriveGeneric #-}
module MTixParReadMerge1 where

import GHC.Generics (Generic)
import System.FilePath

import Control.Monad
import Control.DeepSeq
import Control.Parallel
import Control.Parallel.Strategies

import qualified Data.Sequence as Seq

import Trace.Hpc.Tix
import Trace.Hpc.Util (HpcPos, fromHpcPos, writeFileUtf8, Hash)

import Debug.Trace

------------------------------------------------------------------------------
-- A Tix variant with multiple tickers

-- Ticks count: (ticker, ticks)
type TicksCount = Seq.Seq (String, Integer)

showTicksCount :: TicksCount -> String
showTicksCount ticks =
  "Ticks per property: " <>
  concatMap (\(s,n) -> "&#10" <> s <> " : " <> show n <> " ticks") ticks

-- MTix files contaiting MTixModules
data MTix = MTix [MTixModule]
  deriving (Eq, Show, Read, Generic)

instance NFData MTix

-- MTix modules containing mdoule name, hash and ticks count
data MTixModule = MTixModule String Hash Int [TicksCount]
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

mtixModuleTixs :: MTixModule -> [TicksCount]
mtixModuleTixs (MTixModule  _ _ _ tixs) = tixs

-- Reading and merging MTixs in one parallel operation
readMergeMTixs :: [FilePath] -> IO MTix
readMergeMTixs files = do
  mtixs <- sequence (parMap rpar readMTix files)
  return (mergeMTixs mtixs)

-- Reading MTixs
readMTix :: FilePath -> IO MTix
readMTix file = do
  mtix <- readTix file
  case mtix of
    Nothing -> error $ "error reading tix file from: " ++ file
    Just a -> return (tixToMTix (takeBaseName file) a)

tixToMTix :: String -> Tix -> MTix
tixToMTix ticker (Tix xs) =
  MTix (toMTixModule <$> xs)
  where
    toMTixModule (TixModule n h i ticks) = MTixModule n h i (addTicker <$> ticks)
    addTicker x = Seq.singleton (ticker, x)

-- Merging MTixs
mergeMTixs :: [MTix] -> MTix
mergeMTixs [] = error "mergeMTixs: empty input"
mergeMTixs xs = pfold mergeMTix xs

mergeMTix :: MTix -> MTix -> MTix
mergeMTix (MTix ts1) (MTix ts2) = do
  MTix (zipWith mergeMTixModule ts1 ts2)

mergeMTixModule :: MTixModule -> MTixModule -> MTixModule
mergeMTixModule (MTixModule n1 h1 i1 tks1) (MTixModule n2 h2 i2 tks2)
  | n1 == n2 && h1 == h2 =
      MTixModule n1 h1 (i1 + i2) (zipWith (<>) tks1 tks2)
  | otherwise =
      error $ "mergeMTixs: hash " <> show (h1, h2) <>
              " or module name "  <> show (n1, n2) <> " mismatch"

-- Parallel combinators
parZipWithChunk :: Int -> (a -> a -> a) -> [a] -> [a] -> [a]
parZipWithChunk n f xs ys = zipWith f xs ys `using` parListChunk n rpar

parFold1 :: (a -> a -> a) -> [a] -> a
parFold1 f [x] = x
parFold1 f xs  = parFold1 f (reduce1 f xs `using` parList rpar)

reduce1 :: (a -> a -> a) -> [a] -> [a]
reduce1 _ [] = []
reduce1 _ [x] = [x]
reduce1 f (x:y:ys) = f x y : reduce1 f ys

pfold :: (a -> a -> a) -> [a] -> a
pfold _ [x] = x
pfold f xs  = (ys `par` zs) `pseq` (f ys zs)
  where
    (ys', zs') = splitAt (length xs `div` 2) xs
    ys = pfold f ys'
    zs = pfold f zs'
