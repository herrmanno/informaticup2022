module IC.Data.Train
    ( Train(..)
    , TrainLocation(..)
    , TrainStatus(..)
    , TrainAction(..)
    , hasNoStation
    , hasStation
    , isBoardable
    , makeBoardable
    ) where

import           Control.Lens       (makeLenses)
import           Data.Maybe         (isNothing)
import           IC.Data.Connection (Connection)
import           IC.Data.ID         (ID)
import           IC.Data.Station    (Station)

-- |A Train
data Train = Train
    { t_id       :: ID Train            -- ^ Train ID
    -- |Start station
    --  Must not be `Nothing` if state is used for deriving new states
    , start      :: Maybe (ID Station)  -- ^ start station
    , velocity   :: Double              -- ^ train's velocity
    , t_capacity :: Int                 -- ^ limit of passengers (persons) on train
    } deriving (Eq, Ord, Show)

-- |Returns True if this train has a start station
hasStation :: Train -> Bool
hasStation = not . hasNoStation

-- |Returns True if this train has no start station
hasNoStation :: Train -> Bool
hasNoStation t = isNothing (start t)

-- | Mark status of train in station to distinguish between trains that are boardable and will be boardable
data TrainStatus
    = Arriving          -- ^ train will arrive station at end of round
    | Boardable         -- ^ train is at station and boardable
    deriving (Show, Eq, Ord)

data TrainLocation
    -- |Train is at station
    = TLocStation
        (ID Station)    -- ^ current station
        TrainStatus     -- ^ if the train is ready to board
    -- |Train is on connection
    | TLocConnection
        (ID Connection) -- ^ current location
        (ID Station)    -- ^ station heading to
        Double          -- ^ remaining distance to station
    deriving (Show, Eq, Ord)

-- |Returns True if a train is at a station and boardable
isBoardable :: TrainLocation -> Bool
isBoardable (TLocStation _ Boardable) = True
isBoardable _                         = False

-- | Converts a `TLocStation sid Arriving` into `TLocStation sid Boardable`
--   and returns other train locations as is
makeBoardable :: TrainLocation -> TrainLocation
makeBoardable (TLocStation sid Arriving) = TLocStation sid Boardable
makeBoardable tloc                       = tloc

-- |Actions a train can take
data TrainAction
    -- |Train is set to start at a given station
    = Start Int (ID Station)
    -- |Train leaves station on a given connection
    | Depart Int (ID Connection)
    deriving (Eq, Ord)

instance Show TrainAction where
    show (Start time sid)  = show time <> " Start S" <> show sid
    show (Depart time cid) = show time <> " Depart L" <> show cid

makeLenses ''Train
