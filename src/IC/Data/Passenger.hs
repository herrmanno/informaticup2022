module IC.Data.Passenger (Passenger(..), PassengerLocation(..), isPLocStation, PassengerAction(..)) where

import           Control.Lens    (makeLenses)
import           IC.Data.ID      (ID)
import           IC.Data.Station (Station)
import           IC.Data.Train   (Train)

-- |A passenger
data Passenger = Passenger
    { p_id        :: ID Passenger   -- ^ passenger ID
    , departure   :: ID Station     -- ^ start station
    , destination :: ID Station     -- ^ target station
    , size        :: Int            -- ^ number of persons in this group
    , arrival     :: Int            -- ^ deadline for arrival
    } deriving (Eq, Ord, Show)

-- |Type of a passenger's location
data PassengerLocation
    = PLocStation (ID Station)  -- ^ Passenger is at station
    | PLocTrain (ID Train)      -- ^ Passenger is on train
    deriving (Show, Eq, Ord)

-- |Returns True if a `PassengerLocation` is a `PLocStation`
isPLocStation :: PassengerLocation -> Bool
isPLocStation (PLocStation _) = True
isPLocStation _               = False

-- |Actions a passenger can take
data PassengerAction
    = Board Int (ID Train)  -- ^ Passenger boards a train
    | Detrain Int           -- ^ Passenger leaves a train
    deriving (Eq, Ord)

instance Show PassengerAction where
    show (Board time tid) = show time <> " Board T" <> show tid
    show (Detrain time)   = show time <> " Detrain"

makeLenses ''Passenger
