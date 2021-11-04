{-|
Class of types that provide the context of a specifIC.Planningning problem
-}
module IC.Data.Context.Class (Context(..)) where

import           Data.Set           (Set)
import           IC.Data.Connection (Connection)
import           IC.Data.ID         (ID)
import           IC.Data.Passenger  (Passenger)
import           IC.Data.Station    (Station)
import           IC.Data.Train      (Train)


-- |The class of types that represent a specifIC.Planningning problem's initial configuration
class Context c where
    makeContext :: Set Station -> Set Connection -> Set Train -> Set Passenger -> c
    stations :: c -> Set Station
    connections :: c -> Set Connection
    trains :: c -> Set Train
    passengers :: c -> Set Passenger
    distanceBetween :: c -> ID Station -> ID Station -> Double
