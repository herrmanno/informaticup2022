module IC.Data.Train
    ( Train(..)
    , TrainLocation(..)
    , TrainStatus(..)
    , TrainAction(..)
    , hasNoStation
    , hasStation
    , isBoardable
    , makeBoardable
    , prepareBoarding
    ) where

import Control.Lens (makeLenses)
import Data.Maybe (isNothing)

import IC.Data.Connection (Connection)
import IC.Data.ID (ID)
import IC.Data.Station (Station)

data Train = Train
    { t_id :: ID Train
    , start :: Maybe (ID Station)
    , velocity :: Double
    , t_capacity :: Int
    } deriving (Eq, Ord, Show)

hasStation :: Train -> Bool
hasStation = not . hasNoStation

hasNoStation :: Train -> Bool
hasNoStation t = isNothing (start t)

-- | Mark status of train in station to distinguish between trains that are boardable and will be boardable
data TrainStatus
    = Arriving          -- ^ train will arrive station at end of round
    | WillBeBoardable   -- ^ train stays at station and will be boardable *next round*
    | Boardable         -- ^ train is at station and boardable
    deriving (Show, Eq, Ord)

data TrainLocation = TLocStation
                        (ID Station)    -- ^ current station
                        TrainStatus     -- ^ if the train is ready to board
                   | TLocConnection
                        (ID Connection) -- ^ current location
                        (ID Station)    -- ^ station heading to
                        Double          -- ^ remaining distance to station
                   deriving (Show, Eq, Ord)

isBoardable :: TrainLocation -> Bool
isBoardable (TLocStation _ Boardable) = True
isBoardable _ = False

-- | Converts a `TLocStation sid WillBeBoardable` into `TLocStation sid Boardable`
--   and returns other train locations as is
makeBoardable :: TrainLocation -> TrainLocation
makeBoardable (TLocStation sid WillBeBoardable) = TLocStation sid Boardable
makeBoardable tloc = tloc

-- | Sets TrainStatus=WillBeBoardable on non-boardable train locations
--   Returns `Nothing` is tloc is not a TLocStation
prepareBoarding :: TrainLocation -> Maybe TrainLocation
prepareBoarding tloc@(TLocStation sid Boardable) = Just tloc
prepareBoarding (TLocStation sid _) = Just $ TLocStation sid WillBeBoardable
prepareBoarding tloc = Nothing

data TrainAction = Start Int (ID Station)
                 | Depart Int (ID Connection) 
                 deriving (Eq, Ord)

instance Show TrainAction where
    show (Start time sid) = show time <> " Start S" <> show sid
    show (Depart time cid) = show time <> " Depart L" <> show cid

makeLenses ''Train
