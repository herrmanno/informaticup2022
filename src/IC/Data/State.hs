{-|
Utility type synonyms for `State`
-}
module IC.Data.State (TrainLocations, PassengerLocations, TrainActions, PassengerActions) where

import qualified Data.Map          as M
import           IC.Data.ID        (ID)
import           IC.Data.Passenger (Passenger, PassengerAction,
                                    PassengerLocation)
import           IC.Data.Train     (Train, TrainAction, TrainLocation)

type TrainLocations = M.Map (ID Train) TrainLocation
type PassengerLocations = M.Map (ID Passenger) PassengerLocation
type TrainActions = M.Map (ID Train) [TrainAction]
type PassengerActions = M.Map (ID Passenger) [PassengerAction]
