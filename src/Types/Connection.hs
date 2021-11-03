module Types.Connection (Connection(..)) where

import Control.Lens (makeLenses)
import Types (ID)
import Types.Station (Station)

data Connection = Connection
    { c_id :: ID Connection
    , c_stations :: (ID Station, ID Station)
    , c_capacity :: Int
    , distance :: Double
    } deriving (Eq, Ord, Show)

makeLenses ''Connection