module IC.Data.Connection (Connection(..)) where

import           Control.Lens    (makeLenses)
import           IC.Data.ID      (ID)
import           IC.Data.Station (Station)

-- |A connection between two stations
data Connection = Connection
    { c_id       :: ID Connection               -- ^ connection ID
    , c_stations :: (ID Station, ID Station)    -- ^ connected stations
    , c_capacity :: Int                         -- ^ limit of trains on connection
    , distance   :: Double                      -- ^ length of connection
    } deriving (Eq, Ord, Show)

makeLenses ''Connection
