module IC.Planning.StateSpec (spec) where

import qualified Data.Map                       as M
import qualified Data.Set                       as S
import           IC.Control.MonadPlan           (evalPlan)
import           IC.Data.Connection             (Connection (Connection))
import           IC.Data.Context.DefaultContext (DefaultContext (..),
                                                 emptyContext)
import           IC.Data.Passenger              (Passenger (Passenger),
                                                 PassengerLocation (..))
import           IC.Data.Station                (Station (Station))
import           IC.Data.Train                  (Train (Train),
                                                 TrainLocation (..),
                                                 TrainStatus (..))
import           IC.Planning.State              (State (passengerLocations, trainLocations),
                                                 emptyState, movePassenger,
                                                 movePassengers, moveTrain,
                                                 moveTrains, stateIsFinished,
                                                 stateIsValid,
                                                 trainsInConnection,
                                                 trainsInStation)
import           Test.Hspec                     (SpecWith, describe, it,
                                                 shouldBe)

spec :: SpecWith ()
spec = do
    describe "Accessors" $ do
        it "should return all train (IDs) at given station" $ do
            let st = emptyState {
                trainLocations = M.fromList
                    [ (1, TLocStation 1 Boardable)
                    , (2, TLocStation 1 Boardable)
                    , (3, TLocStation 2 Boardable)
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
                    , (4, TLocStation 0 Boardable)
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
                trainLocations = M.fromList [(1, TLocStation 1 Boardable), (2, TLocConnection 1 1 0)]
            }
            result <- evalPlan (stateIsValid st) ctx
            result `shouldBe` True

        it "should recognize too many trains in station" $ do
            let ctx = emptyContext { _stations = S.singleton (Station 1 1) }
            let st = emptyState {
                trainLocations = M.fromList [(1, TLocStation 1 Boardable), (2, TLocStation 1 Boardable)]
            }
            result <- evalPlan (stateIsValid st) ctx
            result `shouldBe` False

        it "should recognize too many trains in connection" $ do
            let ctx = emptyContext { _connections = S.singleton (Connection 1 (1,1) 1 1) }
            let st = emptyState {
                trainLocations = M.fromList [(1, TLocConnection 1 1 0), (2, TLocConnection 1 1 0)]
            }
            result <- evalPlan (stateIsValid st) ctx
            result `shouldBe` False

        it "should recognize finished state (all passengers at destination)" $ do
            let ctx = emptyContext { _passengers = S.fromList [Passenger 1 1 2 0 0, Passenger 2 2 1 0 0] }
            let st = emptyState {
                passengerLocations = M.fromList [(1,PLocStation 2), (2,PLocStation 1)]
            }
            result <- evalPlan (stateIsFinished st) ctx
            result `shouldBe` True

        it "should recognize non-finished state (all passengers at destination) (1)" $ do
            let ctx = emptyContext { _passengers = S.fromList [Passenger 1 1 2 0 0, Passenger 2 2 1 0 0] }
            let st = emptyState {
                passengerLocations = M.fromList [(1,PLocStation 1), (2,PLocStation 2)]
            }
            result <- evalPlan (stateIsFinished st) ctx
            result `shouldBe` False

        it "should recognize non-finished state (all passengers at destination) (2)" $ do
            let ctx = emptyContext { _passengers = S.fromList [Passenger 1 1 2 0 0] }
            let st = emptyState {
                passengerLocations = M.fromList [(1,PLocTrain 2)]
            }
            result <- evalPlan (stateIsFinished st) ctx
            result `shouldBe` False

    describe "Move train" $ do
        it "train should stay boardable if reamining on station" $ do
            let train = Train 1 Nothing 1 1
            let ctx = emptyContext
            let st = emptyState
                    { trainLocations = M.singleton 1 (TLocStation 1 Boardable) }
            result <- evalPlan (moveTrain train st) ctx
            let target = [ M.singleton 1 (TLocStation 1 Boardable) ]
            S.fromList (trainLocations <$> result) `shouldBe` S.fromList target

        it "train should be convert from arriving -> WillBeBoardable if staying" $ do
            let train = Train 1 Nothing 1 1
            let ctx = emptyContext
            let st = emptyState
                    { trainLocations = M.singleton 1 (TLocStation 1 Arriving) }
            result <- evalPlan (moveTrain train st) ctx
            let target = [ M.singleton 1 (TLocStation 1 WillBeBoardable) ]
            S.fromList (trainLocations <$> result) `shouldBe` S.fromList target

        it "train should stay in station / go on connection / go to next station if possible" $ do
            let train = Train 1 Nothing 1 1
            let ctx = emptyContext
                    { _stations = S.fromList [Station 1 1, Station 2 1]
                    , _connections = S.fromList [Connection 1 (1,2) 1 1, Connection 2 (1,2) 1 2 ]
                    }
            let st = emptyState
                    { trainLocations = M.singleton 1 (TLocStation 1 Boardable) }
            result <- evalPlan (moveTrain train st) ctx
            let target =
                    [ M.singleton 1 (TLocStation 1 Boardable)
                    , M.singleton 1 (TLocConnection 2 2 1)
                    , M.singleton 1 (TLocStation 2 Arriving)
                    ]
            S.fromList (trainLocations <$> result) `shouldBe` S.fromList target

        it "train should stay on connection if velocity < distance to next station " $ do
            let train = Train 1 Nothing 1 1
            let ctx = emptyContext
                    { _connections = S.singleton (Connection 1 (1,2) 1 10)
                    }
            let st = emptyState
                    { trainLocations = M.singleton 1 (TLocConnection 1 2 5) }
            result <- evalPlan (moveTrain train st) ctx
            let target = [ M.singleton 1 (TLocConnection 1 2 4) ]
            S.fromList (trainLocations <$> result) `shouldBe` S.fromList target

        it "train should arrive next station if velocity >= distance to next station" $ do
            let train = Train 1 Nothing 1 1
            let ctx = emptyContext
                    { _connections = S.singleton (Connection 1 (1,2) 1 2)
                    }
            let st = emptyState
                    { trainLocations = M.singleton 1 (TLocConnection 1 2 1) }
            result <- evalPlan (moveTrain train st) ctx
            let target = [ M.singleton 1 (TLocStation 2 Arriving) ]
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
                            [ (1, TLocStation 1 Boardable)
                            , (2, TLocStation 2 Boardable)
                            ]
                    }
            let result = concat $ evalPlan (moveTrains trains st) ctx
            let target =
                    [ M.fromList [(1, TLocStation 1 Boardable), (2, TLocStation 2 Boardable)]
                    , M.fromList [(1, TLocConnection 2 2 1), (2, TLocStation 2 Boardable)]
                    , M.fromList [(1, TLocStation 2 Arriving), (2, TLocStation 2 Boardable)]
                    , M.fromList [(1, TLocStation 1 Boardable), (2, TLocConnection 2 1 1)]
                    , M.fromList [(1, TLocConnection 2 2 1), (2, TLocConnection 2 1 1)]
                    , M.fromList [(1, TLocStation 2 Arriving), (2, TLocConnection 2 1 1)]
                    , M.fromList [(1, TLocStation 1 Boardable), (2, TLocStation 1 Arriving)]
                    , M.fromList [(1, TLocConnection 2 2 1), (2, TLocStation 1 Arriving)]
                    , M.fromList [(1, TLocStation 2 Arriving), (2, TLocStation 1 Arriving)]
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
            result <- evalPlan (movePassenger passenger st) ctx
            let target = [ M.singleton 1 (PLocTrain 1) ]
            S.fromList (passengerLocations <$> result) `shouldBe` S.fromList target

        it "passenger should stay on train if train just entered station (is not boardable)" $ do
            let passenger = Passenger 1 1 2 10 10
            let ctx = emptyContext
            let st = emptyState
                    { passengerLocations = M.singleton 1 (PLocTrain 1)
                    , trainLocations = M.fromList [(1, TLocStation 1 WillBeBoardable), (2, TLocStation 1 WillBeBoardable)]
                    }
            result <- evalPlan (movePassenger passenger st) ctx
            let target = [ M.singleton 1 (PLocTrain 1) ]
            S.fromList (passengerLocations <$> result) `shouldBe` S.fromList target

        it "passenger should stay on train | leave if train is at station and boardable" $ do
            let passenger = Passenger 1 1 2 10 10
            let ctx = emptyContext
            let st = emptyState
                    { passengerLocations = M.singleton 1 (PLocTrain 1)
                    , trainLocations = M.singleton 1 (TLocStation 1 Boardable)
                    }
            result <- evalPlan (movePassenger passenger st) ctx
            let target = [ M.singleton 1 (PLocTrain 1), M.singleton 1 (PLocStation 1) ]
            S.fromList (passengerLocations <$> result) `shouldBe` S.fromList target

        it "passenger should stay on station if there is no train" $ do
            let passenger = Passenger 1 1 2 10 10
            let ctx = emptyContext
            let st = emptyState
                    { passengerLocations = M.singleton 1 (PLocStation 1) }
            result <- evalPlan (movePassenger passenger st) ctx
            let target = [ M.singleton 1 (PLocStation 1) ]
            S.fromList (passengerLocations <$> result) `shouldBe` S.fromList target

        it "passenger should stay on station if there is no boardable train" $ do
            let passenger = Passenger 1 1 2 10 10
            let ctx = emptyContext
            let st = emptyState
                    { passengerLocations = M.singleton 1 (PLocStation 1)
                    , trainLocations = M.fromList [(1, TLocStation 1 WillBeBoardable), (2, TLocStation 1 WillBeBoardable)]
                    }
            result <- evalPlan (movePassenger passenger st) ctx
            let target = [ M.singleton 1 (PLocStation 1) ]
            S.fromList (passengerLocations <$> result) `shouldBe` S.fromList target

        it "passenger should stay on station | board train, if there is one (boardable)" $ do
            let passenger = Passenger 1 1 2 10 10
            let ctx = emptyContext
            let st = emptyState
                    { passengerLocations = M.singleton 1 (PLocStation 1)
                    , trainLocations = M.fromList
                            [ (1, TLocStation 1 Boardable)
                            , (2, TLocStation 1 Boardable)
                            , (3, TLocStation 1 Arriving)
                            ]
                    }
            result <- evalPlan (movePassenger passenger st) ctx
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
                            [ (1, TLocStation 1 Boardable)
                            , (2, TLocConnection 1 2 10)
                            ]
                    }
            let result = concat $ evalPlan (movePassengers passengers st) ctx
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
