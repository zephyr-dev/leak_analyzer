{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleInstances     #-}

module Types where

import           Data.Char                  (isDigit)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Int                   (Int32, Int64)
import           Data.Time.Exts.Base        (Day (..), Hour (..), Micros (..),
                                             Minute (..), Month (..),
                                             Second (..), Year (..))
import           Data.Time.Exts.Parser      (parseUnixDateTimeMicros)
import           Data.Time.Exts.Unix        (UnixDateTimeMicros (..),
                                             createUnixDateTimeMicros)

import           Data.Map                   (Map)
import qualified Data.Map                   as M

import           Data.Text.IO               (readFile)
import Data.Foldable (fold, foldMap)
import Data.Monoid (Monoid (..), (<>))
import Data.List (sort, reverse)

data Entry = RouterEntry {
    timestamp   :: Maybe Timestamp
  , path        :: Text
  , dyno        :: Dyno

  , options     :: Options
} | ProcessRunningMemoryEntry {
    timestamp :: Maybe Timestamp
  , source    :: Source
  , dyno      :: Dyno

  , memsize   :: MemSize
} | MemoryQuotaExceededEntry {
    timestamp :: Maybe Timestamp
  , source    :: Source
  , dyno      :: Dyno
} | UnknownEntry {
    dyno      :: Dyno
  , entry     :: Text
  } deriving Show

type Timestamp = UnixDateTimeMicros

data Source = HerokuSource | AppSource | OtherSource {src :: Text} deriving Show

data Dyno = WebDyno {
    number :: DynoNumber
  } | BackgroundDyno {
    number :: DynoNumber
  } | HerokuRouterDyno 
    | UnknownDyno
  deriving (Eq, Show, Ord)

type DynoNumber = Int
type Options    = Map Text Text
type Path       = Text
type MemSize    = Int
type MemDiff    = Int



newtype MemDiffPathMap = MemDiffPathMap {unMemDiffPathMap :: Map MemDiff [Path]} deriving Show

instance Monoid MemDiffPathMap where
  mempty = MemDiffPathMap M.empty
  MemDiffPathMap m1 `mappend` MemDiffPathMap m2 = MemDiffPathMap $ M.unionWith (++) m1 m2



newtype DynoEntriesMap = DynoEntriesMap {unDynoEntriesMap :: Map Dyno [Entry]} deriving Show

instance Monoid DynoEntriesMap where
  mempty = DynoEntriesMap $ M.empty
  DynoEntriesMap m1 `mappend` DynoEntriesMap m2 = DynoEntriesMap $ M.unionWith (++) m1 m2



data MemDiffPathMapBuildup = MemDiffPathMapBuildup {
    mdpMap        :: MemDiffPathMap
  , lastMemSize   :: Maybe MemSize
  , orphans  :: [Path]
  } deriving Show



newtype MinMemMap = MinMemMap {unMinMemMap :: Map Path MemDiff}

instance Monoid MinMemMap where
  mempty = MinMemMap mempty
  MinMemMap mmm1 `mappend` MinMemMap mmm2 = MinMemMap $ M.unionWith min mmm1 mmm2

{- instance Ord (Path, MemDiff) where -}
  {- (_, md1) `compare` (_, md2) = md1 `compare` md2 -}

instance Show MinMemMap where
  show = unlines . map (\(m, p) -> show m ++ "\t" ++ T.unpack p) . reverse . sort . map (\(x,y) -> (y,x)) . M.toList . unMinMemMap
