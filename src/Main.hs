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
import           Data.Time.Exts.Base        (Day (..), Hour (..), Micros (..),
                                             Minute (..), Month (..),
                                             Second (..), Year (..))
import           Data.Time.Exts.Parser      (parseUnixDateTimeMicros)
import           Data.Time.Exts.Unix        (UnixDateTimeMicros (..),
                                             createUnixDateTimeMicros)

import           Data.Map                   (Map)
import qualified Data.Map                   as M

import           Data.Text.IO               (readFile)
import Data.Maybe (fromJust, catMaybes)
import Data.Foldable (fold, foldMap)
import Data.Monoid (Monoid (..), (<>))
import Data.List (foldl')

import Types
import Parsers (entryParser, dynoParser)



main :: IO ()
main = do
  [logFilename] <- getArgs
  logContent <- readFile logFilename

  putStrLn . show . minMemMap $ logContent
  {- putStrLn $ show $ buildMemDiff $  -}

  where
    minMemMap = buildMinMemMap . unDyno . dynoMemDiffPathsMap
    unDyno = M.foldl' (<>) mempty
    dynoMemDiffPathsHistogram = (M.map buildMemDiffPathsHistogram) . dynoMemDiffPathsMap
    dynoMemDiffPathsMap = (M.map buildMemDiffPathMap) . unDynoEntriesMap . dynoEntryMap
    dynoEntryMap = foldMap (\e -> DynoEntriesMap $ M.singleton (dyno e) [e]) . allEntries
    allEntries = catMaybes . map (\s -> maybeResult $ feed (parse entryParser s) "") . T.lines


buildMinMemMap :: MemDiffPathMap -> MinMemMap
buildMinMemMap =  mconcat . M.foldl' (<>) mempty . M.mapWithKey (\md -> map (\p -> MinMemMap $ M.singleton p md)) . unMemDiffPathMap

buildMemDiffPathsHistogram :: MemDiffPathMap -> [(MemDiff, Int)]
buildMemDiffPathsHistogram = M.toList . M.map length . unMemDiffPathMap

-- assume entries are already ordered by the asc timestamp
buildMemDiffPathMap :: [Entry] -> MemDiffPathMap
buildMemDiffPathMap = mdpMap . foldl' (\mdpmb e ->
  case e of
    RouterEntry{path = p} ->
      case mdpmb of
        MemDiffPathMapBuildup{lastMemSize = Nothing}              -> mdpmb
        MemDiffPathMapBuildup{lastMemSize = Just _, orphans = os} -> mdpmb{orphans = (path e):os}

    ProcessRunningMemoryEntry {memsize = m} ->
      case mdpmb of
        MemDiffPathMapBuildup{lastMemSize = Nothing}              -> mdpmb{              lastMemSize = Just $ memsize e}

        MemDiffPathMapBuildup{mdpMap = mdpm, lastMemSize = Just lm, orphans = os}

          | lm == m                                               -> mdpmb{orphans = []}
          | otherwise                                             -> mdpmb{orphans = [], lastMemSize = Just m, mdpMap = mdpm <> (MemDiffPathMap $ M.singleton (m - lm) os)}

    _                                                             -> mdpmb
  ) (MemDiffPathMapBuildup mempty Nothing [])

{- buildMinMemIncrement :: MemDiffPathMap -> Map MemDiff Entry -}



