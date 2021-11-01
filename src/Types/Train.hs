module Types.Train where

import Control.Lens (makeLenses)
import Data.Maybe (isNothing)
import Types (ID)
import Types.Station (Station)
import Types.Connection (Connection)

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
                   deriving (Show, Eq, Ord)

isBoardable :: TrainLocation -> Bool
isBoardable (TLocStation _ b) = b
isBoardable _ = False

data TrainAction = Start (ID Station)
                 | Depart (ID Connection) 
                 deriving (Show, Eq, Ord)

makeLenses ''Train
