{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
--
-- assume entries are already ordered by the asc timestamp
{- instance Monoid MemDiffPathMapBuildup where -}
  {- mempty = MemDiffPathMapBuildup (MemDiffPathMap M.empty) Nothing [] [] -}

  {- MemDiffPathMapBuildup mdpm1 Nothing los1 ros2 `mappend` MemDiffPathMapBuildup mdpm2 Nothing los2 ros2 =  -}
    {- MemDiffPathMapBuildup (mdpm1 <> mdpm2) Nothing (los1 <> los2) (ros1 <> ros2) -}
  
  {- MemDiffPathMapBuildup mdpm1 Nothing _ `mappend` MemDiffPathMapBuildup mdpm2 (Just lms) os =  -}
    {- MemDiffPathMapBuildup (mdpm1 <> mdpm2) (Just lms) os -}
  {- MemDiffPathMapBuildup mdpm1 (Just lms) os1 `mappend` MemDiffPathMapBuildup mdpm2 Nothing os2 =  -}
    {- MemDiffPathMapBuildup (mdpm1 <> mdpm2) (Just lms) (os1 <> os2) -}
  
  {- MemDiffPathMapBuildup mdpm1 (Just lms1) os1 `mappend` MemDiffPathMapBuildup mdpm2 (Just lms2) os2  -}
    {- | lms1 == lms2  = MemDiffPathMapBuildup (mdpm1 <> mdpm2) (Just lms1) [] -}
    {- | otherwise     = MemDiffPathMapBuildup (mdpm1 <> mdpm2 <> (MemDiffPathMap $ M.singleton (makeMemDiff lms1 lms2) $ os1 <> os2)) (Just lms2) [] -}
    {- where -}
      {- makeMemDiff :: MemSize -> MemSize -> MemDiff -}
      {- makeMemDiff = flip (-) -}


