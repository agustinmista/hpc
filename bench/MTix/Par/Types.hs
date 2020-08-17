{-# LANGUAGE DeriveGeneric #-}
module MTix.Par.Types where

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

mtixModules :: MTix -> [String]
mtixModules (MTix mods) = mtixModuleName <$> mods
