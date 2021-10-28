module Types where

import Control.Lens (makeLenses)
import Data.Functor.Identity (Identity)
import Data.Maybe (isNothing)

type Time = Int

-- TODO: change to newtype to make phantom types work
type ID a = Int

data Station = Station
    { s_id :: ID Station
    , s_capacity :: Int
    } deriving (Eq, Ord, Show)

data Connection = Connection
    { c_id :: ID Connection
    , c_stations :: (ID Station, ID Station)
    , c_capacity :: Int
    , distance :: Double
    } deriving (Eq, Ord, Show)

data Train = Train
    { t_id :: ID Train
    , start :: Maybe (ID Station)
    , velocity :: Double
    , t_capacity :: Int
    } deriving (Eq, Ord, Show)

hasNoStation :: Train -> Bool
hasNoStation t = isNothing (start t)

data TrainLocation = TLocStation
                        (ID Station)    -- ^ current station
                        Bool            -- ^ if the train is ready to board
                   | TLocConnection
                        (ID Connection) -- ^ current location
                        (ID Station)    -- ^ station heading to
                        Double          -- ^ remaining distance to station
                   deriving (Show, Eq)

isBoardable :: TrainLocation -> Bool
isBoardable (TLocStation _ b) = b
isBoardable _ = False

data TrainAction = Start (ID Station)
                 | Depart (ID Connection) 
                 deriving (Show, Eq, Ord)

data Passenger = Passenger
    { p_id :: ID Passenger
    , departure :: ID Station
    , destination :: ID Station
    , size :: Int
    , arrival :: Int
    } deriving (Eq, Ord, Show)

data PassengerLocation = PLocStation (ID Station)
                       | PLocTrain (ID Train)
                       deriving (Show, Eq)

data PassengerAction = Board (ID Train)
                     | Detrain
                     deriving (Show, Eq)

$(makeLenses ''Station)
$(makeLenses ''Connection)
$(makeLenses ''Train)
$(makeLenses ''Passenger)