module PlanResult where

import qualified Data.Map as M
import Types.State (PassengerActions, TrainActions)
import State (State (passengerActions, trainActions))
import Types (ID)
import Types.Train (Train, TrainAction)
import Types.Passenger (Passenger, PassengerAction)
import Data.List (intercalate, sort)

data PlanResult = PlanResult TrainActions PassengerActions

fromState :: State -> PlanResult
fromState s = PlanResult (trainActions s) (passengerActions s)

showTrainActions :: TrainActions -> String
showTrainActions = intercalate "\n" . map f . M.toAscList where
    f (tid, tacs) = unlines $ ("[Train:T" <> show tid <> "]"):map show (sort tacs)

showPassengerActions :: PassengerActions -> String
showPassengerActions = intercalate "\n" . map f . M.toAscList where
    f (pid, pacs) = unlines $ ("[Passenger:P" <> show pid <> "]"):map show (sort pacs)

instance Show PlanResult where
    show (PlanResult tas pas) = unlines [showTrainActions tas, showPassengerActions pas]