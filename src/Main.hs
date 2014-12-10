{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import System.Environment (getArgs)
import           Data.Char                  (isDigit)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Prelude                    hiding (readFile)

import           Data.Attoparsec.Text       (feed, maybeResult, parse)

import           Data.Int                   (Int32, Int64)
import           Data.Time.Exts.Base        (Second (..), plus)
import           Data.Time.Exts.Parser      (parseUnixDateTimeMicros)
import           Data.Time.Exts.Unix        (UnixDateTimeMicros (..),
                                             createUnixDateTimeMicros)

import qualified Data.Map.Strict            as M
import qualified Data.IntMap.Strict         as IM

import           Data.Text.IO               (readFile)
import qualified Data.Foldable as F
import Data.Monoid (Monoid (..), (<>))
import Data.List (foldl')
import           Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (fromJust, catMaybes)
import           Data.Map.Strict (Map)
import           Data.IntMap.Strict (IntMap)

import Types
import Parsers (entryParser, dynoParser)



main :: IO ()
main = do
  [logFilename] <- getArgs
  logContent <- readFile logFilename
  putStrLn . show . minMemMap $ logContent

  where
    minMemMap :: Text -> MinMemMap
    minMemMap = buildMinMemMap . mergeDynos . dynoMemDiffPathsMap
    mergeDynos = M.foldl' (<>) mempty

    dynoMemDiffPathsMap :: Text -> M.Map Dyno MemDiffPathMap
    dynoMemDiffPathsMap = (M.map buildMemDiffPathMap) . dynoEntryMap

    dynoEntryMap :: Text -> Map Dyno (Vector Entry)
    dynoEntryMap =  M.map V.fromList . unDynoEntriesMap . F.foldMap (\e -> DynoEntriesMap $ M.singleton (dyno e) [e]) . allEntries

    allEntries :: Text -> [Entry]
    allEntries = catMaybes . map (\s -> maybeResult $ feed (parse entryParser s) "") . T.lines


buildMinMemMap :: MemDiffPathMap -> MinMemMap
buildMinMemMap =  V.foldl' (<>) mempty . IM.foldl' (<>) mempty . trans . unMemDiffPathMap
  where
    trans :: IntMap (Vector Path) -> IntMap (Vector MinMemMap)
    trans = IM.mapWithKey (\md -> V.map (\p -> MinMemMap $ M.singleton p md))

data MemDiffPathMapBuildup = MemDiffPathMapBuildup {
    mdpMap          :: !MemDiffPathMap
  , lastMemWarning  :: !(Maybe (SP MemSize Timestamp))
  , orphans         :: !(Vector Path)
  }

-- assume entries are already ordered by the asc timestamp
buildMemDiffPathMap :: Vector Entry -> MemDiffPathMap
buildMemDiffPathMap = mdpMap . V.foldl' (\mdpmb e ->
  case e of
    RouterEntry{path = p} ->
      case mdpmb of
        MemDiffPathMapBuildup{lastMemWarning = Nothing}               -> mdpmb
        MemDiffPathMapBuildup{lastMemWarning = Just _, orphans = os}  -> mdpmb{orphans = V.cons (path e) os}

    ProcessRunningMemoryEntry {memsize = m, timestamp = Nothing}      -> mdpmb
    ProcessRunningMemoryEntry {memsize = m, timestamp = Just t} ->
      case mdpmb of
        MemDiffPathMapBuildup{lastMemWarning = Nothing}               -> mdpmb{               lastMemWarning = Just (SP m t)}

        MemDiffPathMapBuildup{mdpMap = mdpm, lastMemWarning = Just (SP lm lt), orphans = os}

          | lm >= m                                                   -> mdpmb{orphans = V.empty} -- disregard all entries within a negative memdiff (for now)
          | lt `plus` Second 30 < t                                   -> mdpmb{orphans = V.empty} -- disregard all entries within a memdiff bucket too large in terms of time
          | otherwise                                                 -> mdpmb{orphans = V.empty,  lastMemWarning = Just (SP m t), mdpMap = mdpm <> (MemDiffPathMap $ IM.singleton (m - lm) os)}

    _                                                                 -> mdpmb
  ) (MemDiffPathMapBuildup mempty Nothing V.empty)


