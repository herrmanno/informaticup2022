module Main (main) where

import Data.Set qualified as S
import Context (emptyContext, ContextType(..))
import State (fromContext)
import Plan (findBestStateRoute)
import Types.Station (Station(Station))
import Types.Connection (Connection(Connection))
import Types.Train (Train(Train))
import Types.Passenger (Passenger(Passenger))
import PlanResult (fromState)

main :: IO ()
main = do
    let ss = fromContext context
    case findBestStateRoute context ss of
        Just result -> print $ fromState result
        Nothing -> print "ERROR: not best plan found"
    where context = example1

example1 :: ContextType
example1 = emptyContext
        { _stations = S.fromList [Station 1 2, Station 2 2, Station 3 2]
        , _connections = S.fromList [Connection 1 (2,3) 1 3.14, Connection 2 (2,1) 1 4]
        , _trains = S.fromList [Train 1 (Just 2) 5.5 30, Train 2 Nothing 0.9999999 50]
        , _passengers = S.fromList [Passenger 1 2 3 3 3, Passenger 2 2 1 10 3]
        }

