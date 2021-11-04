module IC.Data.Station (Station(..)) where

import           Control.Lens (makeLenses)
import           IC.Data.ID   (ID)

-- |A Station
data Station = Station
    { s_id       :: ID Station  -- ^ station ID
    , s_capacity :: Int         -- ^ limit of trains on station
    } deriving (Eq, Ord, Show)

makeLenses ''Station
