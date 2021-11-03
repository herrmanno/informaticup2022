module StateSpec (spec) where

import Test.Hspec ( SpecWith, describe, it, shouldBe )
import App (evalApp)
import Context (ContextType(..), emptyContext)
import Types.Station ( Station(Station) )
import Types.Connection ( Connection(Connection) )
import Types.Train ( TrainLocation(TLocConnection, TLocStation), Train (Train) )
import Types.Passenger ( PassengerLocation(PLocTrain, PLocStation), Passenger(Passenger) )
import State
    ( State(trainLocations, passengerLocations),
      emptyState,
      trainsInStation,
      trainsInConnection,
      stateIsValid,
      moveTrains,
      moveTrain,
      movePassengers,
      movePassenger,
      stateIsFinished )
import Data.Set qualified as S
import Data.Map qualified as M

spec :: SpecWith ()
spec = do
    describe "Accessors" $ do
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

    describe "Validation" $ do
        it "should recognize valid state" $ do
            let ctx = emptyContext
                    { _stations = S.singleton (Station 1 1)
                    , _connections = S.singleton (Connection 1 (1,1) 1 1)
                    }
            let st = emptyState {
                trainLocations = M.fromList [(1, TLocStation 1 False), (2, TLocConnection 1 1 0)]
            }
            result <- evalApp (stateIsValid st) ctx
            result `shouldBe` True

        it "should recognize too many trains in station" $ do
            let ctx = emptyContext { _stations = S.singleton (Station 1 1) }
            let st = emptyState {
                trainLocations = M.fromList [(1, TLocStation 1 False), (2, TLocStation 1 False)]
            }
            result <- evalApp (stateIsValid st) ctx
            result `shouldBe` False

        it "should recognize too many trains in connection" $ do
            let ctx = emptyContext { _connections = S.singleton (Connection 1 (1,1) 1 1) }
            let st = emptyState {
                trainLocations = M.fromList [(1, TLocConnection 1 1 0), (2, TLocConnection 1 1 0)]
            }
            result <- evalApp (stateIsValid st) ctx
            result `shouldBe` False

        it "should recognize finished state (all passengers at destination)" $ do
            let ctx = emptyContext { _passengers = S.fromList [Passenger 1 1 2 0 0, Passenger 2 2 1 0 0] }
            let st = emptyState {
                passengerLocations = M.fromList [(1,PLocStation 2), (2,PLocStation 1)]
            }
            result <- evalApp (stateIsFinished st) ctx
            result `shouldBe` True

        it "should recognize non-finished state (all passengers at destination) (1)" $ do
            let ctx = emptyContext { _passengers = S.fromList [Passenger 1 1 2 0 0, Passenger 2 2 1 0 0] }
            let st = emptyState {
                passengerLocations = M.fromList [(1,PLocStation 1), (2,PLocStation 2)]
            }
            result <- evalApp (stateIsFinished st) ctx
            result `shouldBe` False

        it "should recognize non-finished state (all passengers at destination) (2)" $ do
            let ctx = emptyContext { _passengers = S.fromList [Passenger 1 1 2 0 0] }
            let st = emptyState {
                passengerLocations = M.fromList [(1,PLocTrain 2)]
            }
            result <- evalApp (stateIsFinished st) ctx
            result `shouldBe` False

    describe "Move train" $ do
        it "train should stay in station / go on connection / go to next station if possible" $ do
            let train = Train 1 Nothing 1 1
            let ctx = emptyContext
                    { _stations = S.fromList [Station 1 1, Station 2 1]
                    , _connections = S.fromList [Connection 1 (1,2) 1 1, Connection 2 (1,2) 1 2 ]
                    }
            let st = emptyState
                    { trainLocations = M.singleton 1 (TLocStation 1 False) }
            result <- evalApp (moveTrain train st) ctx
            let target =
                    [ M.singleton 1 (TLocStation 1 True)
                    , M.singleton 1 (TLocConnection 2 2 1)
                    , M.singleton 1 (TLocStation 2 False)
                    ]
            S.fromList (trainLocations <$> result) `shouldBe` S.fromList target

        it "train should stay on connection if velocity < distance to next station " $ do
            let train = Train 1 Nothing 1 1
            let ctx = emptyContext
                    { _connections = S.singleton (Connection 1 (1,2) 1 10)
                    }
            let st = emptyState
                    { trainLocations = M.singleton 1 (TLocConnection 1 2 5) }
            result <- evalApp (moveTrain train st) ctx
            let target = [ M.singleton 1 (TLocConnection 1 2 4) ]
            S.fromList (trainLocations <$> result) `shouldBe` S.fromList target

        it "train should arrive next station if velocity >= distance to next station" $ do
            let train = Train 1 Nothing 1 1
            let ctx = emptyContext
                    { _connections = S.singleton (Connection 1 (1,2) 1 2)
                    }
            let st = emptyState
                    { trainLocations = M.singleton 1 (TLocConnection 1 2 1) }
            result <- evalApp (moveTrain train st) ctx
            let target = [ M.singleton 1 (TLocStation 2 False) ]
            S.fromList (trainLocations <$> result) `shouldBe` S.fromList target

    describe "Move multiple trains" $ do
        it "should return combinations of all train moves" $ do
            let trains = [Train 1 Nothing 1 1, Train 2 Nothing 1 1]
            let ctx = emptyContext
                    { _stations = S.fromList [Station 1 1, Station 2 1]
                    , _connections = S.fromList [Connection 1 (1,2) 1 1, Connection 2 (1,2) 1 2 ]
                    , _trains = S.fromList trains
                    }
            let st = emptyState {
                    trainLocations = M.fromList
                            [ (1, TLocStation 1 False)
                            , (2, TLocStation 2 False)
                            ]
                    }
            let result = concat $ evalApp (moveTrains trains st) ctx
            let target =
                    [ M.fromList [(1, TLocStation 1 True), (2, TLocStation 2 True)]
                    , M.fromList [(1, TLocConnection 2 2 1), (2, TLocStation 2 True)]
                    , M.fromList [(1, TLocStation 2 False), (2, TLocStation 2 True)]
                    , M.fromList [(1, TLocStation 1 True), (2, TLocConnection 2 1 1)]
                    , M.fromList [(1, TLocConnection 2 2 1), (2, TLocConnection 2 1 1)]
                    , M.fromList [(1, TLocStation 2 False), (2, TLocConnection 2 1 1)]
                    , M.fromList [(1, TLocStation 1 True), (2, TLocStation 1 False)]
                    , M.fromList [(1, TLocConnection 2 2 1), (2, TLocStation 1 False)]
                    , M.fromList [(1, TLocStation 2 False), (2, TLocStation 1 False)]
                    ]
            S.fromList (trainLocations <$> result) `shouldBe` S.fromList target

    describe "Move passenger" $ do
        it "passenger should stay on train if train is on connection" $ do
            let passenger = Passenger 1 1 2 10 10
            let ctx = emptyContext
            let st = emptyState
                    { passengerLocations = M.singleton 1 (PLocTrain 1)
                    , trainLocations = M.singleton 1 (TLocConnection 1 2 10)
                    }
            result <- evalApp (movePassenger passenger st) ctx
            let target = [ M.singleton 1 (PLocTrain 1) ]
            S.fromList (passengerLocations <$> result) `shouldBe` S.fromList target

        it "passenger should stay on train if train just entered station (is not boardable)" $ do
            let passenger = Passenger 1 1 2 10 10
            let ctx = emptyContext
            let st = emptyState
                    { passengerLocations = M.singleton 1 (PLocTrain 1)
                    , trainLocations = M.singleton 1 (TLocStation 1 False)
                    }
            result <- evalApp (movePassenger passenger st) ctx
            let target = [ M.singleton 1 (PLocTrain 1) ]
            S.fromList (passengerLocations <$> result) `shouldBe` S.fromList target

        it "passenger should stay on train | leave if train is at station and boardable" $ do
            let passenger = Passenger 1 1 2 10 10
            let ctx = emptyContext
            let st = emptyState
                    { passengerLocations = M.singleton 1 (PLocTrain 1)
                    , trainLocations = M.singleton 1 (TLocStation 1 True)
                    }
            result <- evalApp (movePassenger passenger st) ctx
            let target = [ M.singleton 1 (PLocTrain 1), M.singleton 1 (PLocStation 1) ]
            S.fromList (passengerLocations <$> result) `shouldBe` S.fromList target

        it "passenger should stay on station if there is no train" $ do
            let passenger = Passenger 1 1 2 10 10
            let ctx = emptyContext
            let st = emptyState
                    { passengerLocations = M.singleton 1 (PLocStation 1) }
            result <- evalApp (movePassenger passenger st) ctx
            let target = [ M.singleton 1 (PLocStation 1) ]
            S.fromList (passengerLocations <$> result) `shouldBe` S.fromList target

        it "passenger should stay on station if there is no boardable train" $ do
            let passenger = Passenger 1 1 2 10 10
            let ctx = emptyContext
            let st = emptyState
                    { passengerLocations = M.singleton 1 (PLocStation 1)
                    , trainLocations = M.singleton 1 (TLocStation 1 False)
                    }
            result <- evalApp (movePassenger passenger st) ctx
            let target = [ M.singleton 1 (PLocStation 1) ]
            S.fromList (passengerLocations <$> result) `shouldBe` S.fromList target

        it "passenger should stay on station | board train, if there is one (boardable)" $ do
            let passenger = Passenger 1 1 2 10 10
            let ctx = emptyContext
            let st = emptyState
                    { passengerLocations = M.singleton 1 (PLocStation 1)
                    , trainLocations = M.fromList
                            [ (1, TLocStation 1 True)
                            , (2, TLocStation 1 True)
                            , (3, TLocStation 1 False)
                            ]
                    }
            result <- evalApp (movePassenger passenger st) ctx
            let target =
                    [ M.singleton 1 (PLocStation 1)
                    , M.singleton 1 (PLocTrain 1)
                    , M.singleton 1 (PLocTrain 2)
                    ]
            S.fromList (passengerLocations <$> result) `shouldBe` S.fromList target

    describe "Move multiple passengers" $ do
        it "should return combinations of all passenger moves" $ do
            let passengers =
                    [ Passenger 1 1 2 10 10
                    , Passenger 2 1 2 10 10
                    , Passenger 3 1 2 10 10
                    , Passenger 4 1 2 10 10
                    ]
            let ctx = emptyContext
            let st = emptyState
                    { passengerLocations = M.fromList
                            [ (1, PLocStation 1)
                            , (2, PLocStation 2)
                            , (3, PLocTrain 1)
                            , (4, PLocTrain 2)
                            ]
                    , trainLocations = M.fromList
                            [ (1, TLocStation 1 True)
                            , (2, TLocConnection 1 2 10)
                            ]
                    }
            let result = concat $ evalApp (movePassengers passengers st) ctx
            let target =
                    [ M.fromList
                        [ (1, PLocStation 1)
                        , (2, PLocStation 2)
                        , (3, PLocTrain 1)
                        , (4, PLocTrain 2)
                        ]
                    , M.fromList
                        [ (1, PLocTrain 1)
                        , (2, PLocStation 2)
                        , (3, PLocTrain 1)
                        , (4, PLocTrain 2)
                        ]
                    , M.fromList
                        [ (1, PLocStation 1)
                        , (2, PLocStation 2)
                        , (3, PLocStation 1)
                        , (4, PLocTrain 2)
                        ]
                    , M.fromList
                        [ (1, PLocTrain 1)
                        , (2, PLocStation 2)
                        , (3, PLocStation 1)
                        , (4, PLocTrain 2)
                        ]
                    ]
            S.fromList (passengerLocations <$> result) `shouldBe` S.fromList target