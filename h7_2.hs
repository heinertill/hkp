import Control.Monad.State

-- Define the state type
type CounterState = Int

-- Define a simple stateful operation to increment the counter
incrementCounter :: State CounterState ()
incrementCounter = modify (+1)

-- Define a simple stateful operation to decrement the counter
decrementCounter :: State CounterState ()
decrementCounter = modify (subtract 1)

-- Define a stateful operation to get the current counter value
getCounter :: State CounterState Int
getCounter = get

-- Run a sequence of stateful operations using the State monad
runOperations :: State CounterState Int
runOperations = do
  incrementCounter
  incrementCounter
  decrementCounter
  getCounter


main :: IO ()
main = do 
  let initialState = 0
  let (result, finalState) = runState runOperations initialState
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Final Counter State: " ++ show finalState