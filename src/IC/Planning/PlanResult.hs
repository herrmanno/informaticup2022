{-|
Utilities for showing the result of train plan problem's solution
-}
module IC.Planning.PlanResult (PlanResult, fromState) where

import           Data.List         (intercalate, sort)
import qualified Data.Map          as M
import           IC.Data.State     (PassengerActions, TrainActions)
import           IC.Planning.State (State (passengerActions, trainActions))

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
