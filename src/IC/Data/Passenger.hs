module IC.Data.Passenger (Passenger(..), PassengerLocation(..), isPLocStation, PassengerAction(..)) where

import Control.Lens (makeLenses)

import IC.Data.ID (ID)
import IC.Data.Station (Station)
import IC.Data.Train (Train)

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

isPLocStation :: PassengerLocation -> Bool
isPLocStation (PLocStation _) = True
isPLocStation _ = False

data PassengerAction = Board Int (ID Train)
                     | Detrain Int
                     deriving (Eq, Ord)

instance Show PassengerAction where
    show (Board time tid) = show time <> " Board T" <> show tid
    show (Detrain time) = show time <> " Detrain"

makeLenses ''Passenger