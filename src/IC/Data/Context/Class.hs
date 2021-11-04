{-|
Class of types that provide the context of a specific planning problem
-}
module IC.Data.Context.Class (Context(..)) where

import           Data.Set           (Set)
import           IC.Data.Connection (Connection)
import           IC.Data.ID         (ID)
import           IC.Data.Passenger  (Passenger)
import           IC.Data.Station    (Station)
import           IC.Data.Train      (Train)


-- |The class of types that represent a specific planning problem's initial configuration
class Context c where
    -- |Creates a context
    makeContext :: Set Station -> Set Connection -> Set Train -> Set Passenger -> c
    -- |Returns the context's stations
    stations :: c -> Set Station
    -- |Returns the context's connections
    connections :: c -> Set Connection
    -- |Returns the context's trains
    trains :: c -> Set Train
    -- |Returns the context's passengers
    passengers :: c -> Set Passenger
    -- |Returns the distance between two stations
    distanceBetween :: c -> ID Station -> ID Station -> Double
