{-|
Class of types that provide the context of a specifIC.Planningning problem
-}
module IC.Data.Context.Class (Context(..)) where

import Data.Set qualified as S

import IC.Data.Connection ( Connection )
import IC.Data.ID (ID)
import IC.Data.Passenger ( Passenger )
import IC.Data.Station ( Station )
import IC.Data.Train ( Train )


-- |The class of types that represent a specifIC.Planningning problem's initial configuration
class Context c where
    makeContext :: S.Set Station -> S.Set Connection -> S.Set Train -> S.Set Passenger -> c
    stations :: c -> S.Set Station
    connections :: c -> S.Set Connection
    trains :: c -> S.Set Train
    passengers :: c -> S.Set Passenger
    distanceBetween :: c -> ID Station -> ID Station -> Double