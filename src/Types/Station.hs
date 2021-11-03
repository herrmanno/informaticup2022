module Types.Station (Station(..)) where

import Control.Lens (makeLenses)
import Types (ID)

data Station = Station
    { s_id :: ID Station
    , s_capacity :: Int
    } deriving (Eq, Ord, Show)

makeLenses ''Station