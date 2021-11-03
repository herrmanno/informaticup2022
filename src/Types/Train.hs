module Types.Train (Train(..), hasNoStation, hasStation, TrainLocation(..), isBoardable, TrainAction(..)) where

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

hasStation :: Train -> Bool
hasStation = not . hasNoStation

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

data TrainAction = Start Int (ID Station)
                 | Depart Int (ID Connection) 
                 deriving (Eq, Ord)

instance Show TrainAction where
    show (Start time sid) = show time <> " Start S" <> show sid
    show (Depart time cid) = show time <> " Line L" <> show cid

makeLenses ''Train
