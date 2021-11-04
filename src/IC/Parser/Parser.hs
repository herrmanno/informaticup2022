module IC.Parser.Parser (parseContext) where

import           Control.Monad                 (void)
import qualified Data.Set                      as S
import           IC.Data.Connection            (Connection (..))
import           IC.Data.Context.Class         (Context (makeContext))
import           IC.Data.ID                    (ID (ID))
import           IC.Data.Passenger             (Passenger (..))
import           IC.Data.Station               (Station (Station))
import           IC.Data.Train                 (Train (..))
import           Text.Parsec                   (ParseError, Parsec, anyChar,
                                                char, digit, endOfLine, eof,
                                                many, many1, noneOf,
                                                optionMaybe, optional, parse,
                                                spaces, string, (<|>))
import           Text.ParserCombinators.Parsec (Parser)

parseContext :: Context c => String -> Either ParseError c
parseContext = parse context "context input"

context :: Context c => Parsec String u c
context = do
  junk
  ss <- stations
  junk
  cs <- connections
  junk
  ts <- trains
  junk
  ps <- passengers
  junk
  eof
  return $ makeContext ss cs ts ps
  where
    junk = many (comment <|> void endOfLine)

stations :: Parsec String u (S.Set Station)
stations = do
  line $ string "[Stations]"
  ss <- many1 station
  return $ S.fromList ss
  where
    station = Station <$> stationID <*> stationCapacity
    stationID = ID <$> (char 'S' *> number <* spaces)
    stationCapacity = number <* spaces

connections :: Parsec String u (S.Set Connection)
connections = do
  line $ string "[Lines]"
  cs <- many1 connection
  return $ S.fromList cs
  where
    connection = do
      c_id <- connectionID
      c_stations <- (,) <$> stationID <*> stationID
      distance <- number <* spaces
      c_capacity <- number <* spaces
      return $ Connection { c_id, c_stations, distance, c_capacity }
    connectionID = ID <$> (char 'L' *> number <* spaces)
    stationID = ID <$> (char 'S' *> number <* spaces)

trains :: Parsec String u (S.Set Train)
trains = do
  line $ string "[Trains]"
  ts <- many1 train
  return $ S.fromList ts
  where
    train = do
      t_id <- trainID
      start <- startStation
      velocity <- number <* spaces
      t_capacity <- number <* spaces
      return $ Train { t_id, start, velocity, t_capacity }
    trainID = ID <$> (char 'T' *> number <* spaces)
    startStation = (Just <$> stationID <|> Nothing <$ char '*') <* spaces
    stationID = ID <$> (char 'S' *> number <* spaces)

passengers :: Parsec String u (S.Set Passenger)
passengers = do
  line $ string "[Passengers]"
  ps <- many1 passenger
  return $ S.fromList ps
  where
    passenger = do
      p_id <- passengerID
      departure <- stationID
      destination <- stationID
      size <- number <* spaces
      arrival <- number <* spaces
      return $ Passenger { p_id, departure, destination, size, arrival }
    passengerID = ID <$> (char 'P' *> number <* spaces)
    stationID = ID <$> (char 'S' *> number <* spaces)

-----------------------------------------------------------
--                  UTILITIES
-----------------------------------------------------------

line :: Parsec String u a -> Parsec String u a
line p = p <* (spaces >> optional comment)

number :: (Num a, Read a) => Parsec String u a
number = toNum <$> many1 digit <*> optionMaybe (char '.' *> many1 digit)
  where
    toNum a (Just b) = read $ a <> "." <> b
    toNum a _        = read a

comment :: Parsec String u ()
comment = char '#' >> many (noneOf "\n") >> endOfLine >> return ()
