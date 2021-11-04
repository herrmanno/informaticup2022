module Main (main) where

import           IC.Data.Context        (Context, DefaultContext (..))
import           IC.Parser.Parser       (parseContext)
import           IC.Planning.Plan       (findBestStateRoute)
import           IC.Planning.PlanResult (fromState)
import           IC.Planning.State      (fromContext)


main :: IO ()
main = do
    eitherC <- (parseContext @DefaultContext) <$> getContents
    case eitherC of
        Left err -> print err
        Right c  -> run c

run :: Context c => c -> IO ()
run context = do
    let ss = fromContext context
    case findBestStateRoute context ss of
        Just result -> print $ fromState result
        Nothing     -> print "ERROR: not best plan found"
