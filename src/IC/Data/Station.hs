module IC.Data.Station (Station(..)) where

import Control.Lens (makeLenses)

import IC.Data.ID (ID)

data Station = Station
    { s_id :: ID Station
    , s_capacity :: Int
    } deriving (Eq, Ord, Show)

makeLenses ''Station