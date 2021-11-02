module Plan where

import Prelude hiding (head)
import App (App, ContextT, evalApp)
import Context (Context)
import State (State, scoreForState, nextStates, stateIsValid, stateIsFinished, Score)
import Data.Heap (Heap, HeapItem, Prio, Val)
import qualified Data.Heap as H
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NL
import Data.Functor.Identity (runIdentity, Identity)
import Debug.Trace

data StateScore

instance HeapItem StateScore (State, Score) where
    type Val StateScore (State, Score) = State
    newtype Prio StateScore (State, Score) = P { prio :: Score } deriving (Eq, Ord)

    split (s,sc) = (P sc, s)
    merge (P sc,s) = (s,sc)

findBestStateRoute :: Context c => c -> [State] -> Maybe State
findBestStateRoute c s = go (H.fromList (fmap stateWithScore s)) where
    go :: Heap StateScore (State, Score) -> Maybe State
    go heap =
        let Just ((s,score), heap') = H.view heap
            ss' = [ stateWithScore s'
                  | ss <- evalApp (nextStates s) c
                  , s' <- ss
                  , valid <- evalApp (stateIsValid s') c
                  , valid
                  ]
        in if runIdentity $ evalApp (stateIsFinished s) c
            then Just s
            else go (foldr H.insert heap' ss')
    stateWithScore s = runIdentity $ evalApp ((s,) <$> scoreForState s) c
