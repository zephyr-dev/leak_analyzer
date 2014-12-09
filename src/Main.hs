{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Char (isDigit)
import Control.Applicative (Alternative, pure, liftA2, (<$>), (<*>), (<*), (*>), (<|>))
import Data.Text (Text)
import qualified Data.Text as T

import Data.Attoparsec.Combinator (many', manyTill)
import Data.Attoparsec.Text (Parser (..), many1, char, isEndOfLine, skipWhile, string, takeTill, parse, digit, endOfLine, endOfInput, isEndOfLine, sepBy, maybeResult, try, anyChar)

import qualified Data.Attoparsec.Text as AP

import Data.Time.Exts.Parser (parseUnixDateTimeMicros)
import Data.Time.Exts.Unix (UnixDateTimeMicros(..), createUnixDateTimeMicros)
import Data.Time.Exts.Base (Year(..), Month(..), Day(..), Hour(..), Minute(..), Second(..), Micros(..))
import Data.Int (Int32, Int64)

import Data.Map (Map)
import qualified Data.Map as M


data Entry = Router {
    timestamp   :: Maybe Timestamp
  , options     :: Options

  , path        :: Text
  , status      :: Status
} | ProcessRunningMemory {
    timestamp   :: Maybe Timestamp
  , source      :: Source
  , dyno        :: Dyno

  , memsize     :: Int
} | MemoryQuotaExceeded {
    timestamp   :: Maybe Timestamp
  , source      :: Source
  , dyno        :: Dyno
} | UnknownEntry {
    entry       :: Text
  } deriving Show

type Timestamp = UnixDateTimeMicros

data Source = HerokuSource | AppSource | OtherSource {src :: Text} deriving Show

data Dyno = Web {
    number      :: DynoNumber
  } | Background {
    number      :: DynoNumber
  } | HerokuRouter
  deriving Show

type DynoNumber = Int
type Status = Int

type Options = Map Text Text

{- data Code = H18 |  -}

main :: IO ()
main = do
  logContent <- readFile "log"

  putStrLn $ show $ map (maybeResult . parse entryParser . T.pack) $ lines logContent


entryParser :: Parser Entry
entryParser = (try routerParser) <|>
              (try processRunningMemoryParser) <|>
              (try memoryQuotaExceededParser) <|>
              unknownEntryParser


routerParser :: Parser Entry
routerParser = do
  (ts, HerokuSource, HerokuRouter) <- timestampSourceAndDynoParser
  opts <- optionsParser
  return $ Router {
      timestamp = ts
    , options = opts
    , path = ""
    , status = 200
    }

processRunningMemoryParser :: Parser Entry
processRunningMemoryParser = do
  (ts, src, dyn) <- timestampSourceAndDynoParser
  string "Process running mem="
  mem <- intParser
  return $ ProcessRunningMemory {
      timestamp = ts
    , source = src
    , dyno = dyn
    , memsize = mem
    }

memoryQuotaExceededParser :: Parser Entry
memoryQuotaExceededParser = do
  (ts, src, dyn) <- timestampSourceAndDynoParser
  string "Error R14 (Memory quota exceeded)"
  return $ MemoryQuotaExceeded {
      timestamp = ts
    , source = src
    , dyno = dyn
    }

unknownEntryParser :: Parser Entry
unknownEntryParser = pure $ UnknownEntry "unknown"

optionsParser :: Parser Options
optionsParser = do
  opts <- many' optionParser
  return $ M.fromList opts

optionParser :: Parser (Text, Text)
optionParser = (,) <$> (takeTill (=='=') <* char '=') <*> consumeValue

consumeValue :: Parser Text
consumeValue = (char '"' *> takeTill (=='"') <* char '"') <|> consumeField

timestampSourceAndDynoParser :: Parser (Maybe Timestamp, Source, Dyno)
timestampSourceAndDynoParser = (,,) <$> (consumeField *> consumeField *> timestampParser <* char ' ') <*> sourceParser <*> (dynoParser <* separatorParser)


separatorParser = string " - - "

dynoParser :: Parser Dyno
dynoParser = routerParser <|> webParser <|> backgroundParser
  where
    webParser         = Web                   <$> (string "web"        *> (char '.' *> intParser))
    backgroundParser  = Background            <$> (string "background" *> (char '.' *> intParser))
    routerParser      = (\_ -> HerokuRouter)  <$> (string "router")

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


{- timestampParser :: Parser UnixDateTimeMicros -}
{- timestampParser = -}
  {- createUnixDateTimeMicros <$> -}
        {- (Year   <$> intParser) <* char '-' -}
    {- <*> (Month  <$> intParser) <* char '-' -}
    {- <*> (Day    <$> intParser) <* char 'T' -}
    {- <*> (Hour   <$> intParser) <* char ':' -}
    {- <*> (Minute <$> intParser) <* char ':' -}
    {- <*> (Second <$> intParser) <* char '.' -}
    {- <*> (Micros <$> intParser) <* (takeTill (==' ')) -}
    
intParser :: Parser Int
intParser = read . T.unpack <$> AP.takeWhile isDigit
