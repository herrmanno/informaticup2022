module StateSpec (spec) where

import Test.Hspec ( describe, it, shouldBe )
import Context (ContextType(..), emptyContext)
import Types.Station ( Station(Station) )
import Types.Connection ( Connection(Connection) )
import Types.Train ( TrainLocation(TLocConnection, TLocStation), Train (Train) )
import Types.Passenger ( PassengerLocation(PLocTrain, PLocStation), Passenger(Passenger) )
import State
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Trans.State (evalStateT)

spec = do
    describe "State" $ do
        it "should return all train (IDs) at given station" $ do
            let st = emptyState {
                trainLocations = M.fromList
                    [ (1, TLocStation 1 False)
                    , (2, TLocStation 1 False)
                    , (3, TLocStation 2 False)
                    , (4, TLocConnection 0 0 0)
                    ]
            }
            trainsInStation st 1 `shouldBe` [1, 2]
            trainsInStation st 2 `shouldBe` [3]
            trainsInStation st 3 `shouldBe` []

        it "should return all train (IDs) at given connection" $ do
            let st = emptyState {
                trainLocations = M.fromList
                    [ (1, TLocConnection 1 0 0)
                    , (2, TLocConnection 1 0 0)
                    , (3, TLocConnection 2 0 0)
                    , (4, TLocStation 0 False)
                    ]
            }
            trainsInConnection st 1 `shouldBe` [1, 2]
            trainsInConnection st 2 `shouldBe` [3]
            trainsInConnection st 3 `shouldBe` []
        
        it "should recognize valid state" $ do
            let ctx = emptyContext
                    { _stations = S.singleton (Station 1 1)
                    , _connections = S.singleton (Connection 1 (1,1) 1 1)
                    }
            let st = emptyState {
                trainLocations = M.fromList [(1, TLocStation 1 False), (2, TLocConnection 1 1 0)]
            }
            result <- evalStateT (stateIsValid st) ctx
            result `shouldBe` True

        it "should recognize too many trains in station" $ do
            let ctx = emptyContext { _stations = S.singleton (Station 1 1) }
            let st = emptyState {
                trainLocations = M.fromList [(1, TLocStation 1 False), (2, TLocStation 1 False)]
            }
            result <- evalStateT (stateIsValid st) ctx
            result `shouldBe` False

        it "should recognize too many trains in connection" $ do
            let ctx = emptyContext { _connections = S.singleton (Connection 1 (1,1) 1 1) }
            let st = emptyState {
                trainLocations = M.fromList [(1, TLocConnection 1 1 0), (2, TLocConnection 1 1 0)]
            }
            result <- evalStateT (stateIsValid st) ctx
            result `shouldBe` False

        it "should recognize finished state (all passengers at destination)" $ do
            let ctx = emptyContext { _passengers = S.fromList [Passenger 1 1 2 0 0, Passenger 2 2 1 0 0] }
            let st = emptyState {
                passengerLocations = M.fromList [(1,PLocStation 2), (2,PLocStation 1)]
            }
            result <- evalStateT (stateIsFinished st) ctx
            result `shouldBe` True

        it "should recognize non-finished state (all passengers at destination) (1)" $ do
            let ctx = emptyContext { _passengers = S.fromList [Passenger 1 1 2 0 0, Passenger 2 2 1 0 0] }
            let st = emptyState {
                passengerLocations = M.fromList [(1,PLocStation 1), (2,PLocStation 2)]
            }
            result <- evalStateT (stateIsFinished st) ctx
            result `shouldBe` False

        it "should recognize non-finished state (all passengers at destination) (2)" $ do
            let ctx = emptyContext { _passengers = S.fromList [Passenger 1 1 2 0 0] }
            let st = emptyState {
                passengerLocations = M.fromList [(1,PLocTrain 2)]
            }
            result <- evalStateT (stateIsFinished st) ctx
            result `shouldBe` False

        it "should move train to next possible locations (1)" $ do
            let train = Train 1 Nothing 1 1
            let ctx = emptyContext
                    { _stations = S.fromList [Station 1 1, Station 2 1]
                    , _connections = S.fromList [Connection 1 (1,2) 1 1, Connection 2 (1,2) 1 2 ]
                    }
            let st = emptyState
                    { trainLocations = M.singleton 1 (TLocStation 1 False) }
            result <- evalStateT (moveTrain train st) ctx
            let target =
                    [ M.singleton 1 (TLocStation 1 True)
                    , M.singleton 1 (TLocConnection 2 2 1)
                    , M.singleton 1 (TLocStation 2 False)
                    ]
            S.fromList (trainLocations <$> result) `shouldBe` S.fromList target

        it "should move train to next possible locations (2)" $ do
            let train = Train 1 Nothing 1 1
            let ctx = emptyContext
                    { _connections = S.singleton (Connection 1 (1,2) 1 10)
                    }
            let st = emptyState
                    { trainLocations = M.singleton 1 (TLocConnection 1 2 5) }
            result <- evalStateT (moveTrain train st) ctx
            let target = [ M.singleton 1 (TLocConnection 1 2 4) ]
            S.fromList (trainLocations <$> result) `shouldBe` S.fromList target

        it "should move train to next possible locations (3)" $ do
            let train = Train 1 Nothing 1 1
            let ctx = emptyContext
                    { _connections = S.singleton (Connection 1 (1,2) 1 2)
                    }
            let st = emptyState
                    { trainLocations = M.singleton 1 (TLocConnection 1 2 1) }
            result <- evalStateT (moveTrain train st) ctx
            let target = [ M.singleton 1 (TLocStation 2 False) ]
            S.fromList (trainLocations <$> result) `shouldBe` S.fromList target
