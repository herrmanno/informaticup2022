module Types.Passenger where

import Control.Lens (makeLenses)
import Types (ID)
import Types.Station (Station)
import Types.Train (Train)

data Passenger = Passenger
    { p_id :: ID Passenger
    , departure :: ID Station
    , destination :: ID Station
    , size :: Int
    , arrival :: Int
    } deriving (Eq, Ord, Show)

data PassengerLocation = PLocStation (ID Station)
                       | PLocTrain (ID Train)
                       deriving (Show, Eq, Ord)

data PassengerAction = Board (ID Train)
                     | Detrain
                     deriving (Show, Eq, Ord)

makeLenses ''Passenger