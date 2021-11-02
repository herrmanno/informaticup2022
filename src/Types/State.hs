module Types.State where
import qualified Data.Map as M
import Types (ID)
import Types.Train ( Train, TrainAction, TrainLocation )
import Types.Passenger ( Passenger, PassengerAction, PassengerLocation )

type  TrainLocations = M.Map (ID Train) TrainLocation
type  PassengerLocations = M.Map (ID Passenger) PassengerLocation
type  TrainActions = M.Map (ID Train) [TrainAction]
type  PassengerActions = M.Map (ID Passenger) [PassengerAction]
