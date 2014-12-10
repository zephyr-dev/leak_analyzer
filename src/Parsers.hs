{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parsers where

import           Control.Applicative        (Alternative, liftA2, pure, (*>),
                                             (<$>), (<*), (<*>), (<|>))
import           Data.Char                  (isDigit)
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Data.Attoparsec.Combinator (many', manyTill)
import           Data.Attoparsec.Text       (Parser (..), anyChar, char, digit,
                                             endOfInput, endOfLine, feed,
                                             isEndOfLine, isEndOfLine, many1,
                                             maybeResult, parse, sepBy,
                                             skipWhile, string, takeTill, try)

import qualified Data.Attoparsec.Text       as AP

import           Data.Int                   (Int32, Int64)
import           Data.Time.Exts.Base        (Day (..), Hour (..), Micros (..),
                                             Minute (..), Month (..),
                                             Second (..), Year (..))
import           Data.Time.Exts.Parser      (parseUnixDateTimeMicros)
import           Data.Time.Exts.Unix        (UnixDateTimeMicros (..),
                                             createUnixDateTimeMicros)

import           Data.Map                   (Map)
import qualified Data.Map                   as M
import Data.Maybe (fromJust, catMaybes)

import Types

entryParser :: Parser Entry
entryParser = (try routerEntryParser) <|>
              (try processRunningMemoryEntryParser) <|>
              (try memoryQuotaExceededEntryParser) <|>
              unknownEntryParser


routerEntryParser :: Parser Entry
routerEntryParser = do
  (ts, HerokuSource, HerokuRouterDyno) <- timestampSourceAndDynoParser
  opts <- optionsParser
  return $ RouterEntry {
      timestamp = ts
    , dyno      = fromJust $ (maybeResult . (flip feed) "" . parse dynoParser) =<< (M.lookup "dyno" opts)
    , path      = fromJust $ M.lookup "path" opts
    , options   = opts
    }

processRunningMemoryEntryParser :: Parser Entry
processRunningMemoryEntryParser = do
  (ts, src, dyn) <- timestampSourceAndDynoParser
  string "Process running mem="
  mem <- intParser
  return $ ProcessRunningMemoryEntry {
      timestamp = ts
    , source = src
    , dyno = dyn
    , memsize = mem
    }

memoryQuotaExceededEntryParser :: Parser Entry
memoryQuotaExceededEntryParser = do
  (ts, src, dyn) <- timestampSourceAndDynoParser
  string "Error R14 (Memory quota exceeded)"
  return $ MemoryQuotaExceededEntry {
      timestamp = ts
    , source = src
    , dyno = dyn
    }

unknownEntryParser :: Parser Entry
unknownEntryParser = (UnknownEntry UnknownDyno) <$> (AP.takeWhile (\_ -> True))

optionsParser :: Parser Options
optionsParser = do
  opts <- sepBy optionParser (char ' ')
  return $ M.fromList opts

optionParser :: Parser (Text, Text)
optionParser = (,) <$> (takeTill (=='=') <* char '=') <*> consumeValue

consumeValue :: Parser Text
consumeValue = (char '"' *> takeTill (=='"') <* char '"') <|> takeTill (==' ')

timestampSourceAndDynoParser :: Parser (Maybe Timestamp, Source, Dyno)
timestampSourceAndDynoParser = (,,) <$> (consumeField *> consumeField *> timestampParser <* char ' ') <*> sourceParser <*> (dynoParser <* separatorParser)


separatorParser = string " - - "

dynoParser :: Parser Dyno
dynoParser = routerParser <|> webParser <|> backgroundParser
  where
    webParser         = WebDyno                   <$> (string "web"        *> (char '.' *> intParser))
    backgroundParser  = BackgroundDyno            <$> (string "background" *> (char '.' *> intParser))
    routerParser      = (\_ -> HerokuRouterDyno)  <$> (string "router")

sourceParser :: Parser Source
sourceParser = do
  src <- consumeField
  return $ case src of
    "heroku"  -> HerokuSource
    "app"     -> AppSource
    _         -> OtherSource src

timestampParser :: Parser (Maybe Timestamp)
timestampParser = do
  formattedTimestamp <- takeTill (=='+')
  crap <- char '+' >> digit >> digit >> char ':' >> digit >> digit
  return $ eitherToMaybe $ parseUnixDateTimeMicros "%FT%T.%Q" formattedTimestamp

  where
    eitherToMaybe (Left _)  = Nothing
    eitherToMaybe (Right x) = Just x

consumeField :: Parser Text
consumeField = do
  content <- takeTill (==' ')
  char ' '
  return content

intParser :: Parser Int
intParser = read . T.unpack <$> AP.takeWhile isDigit
