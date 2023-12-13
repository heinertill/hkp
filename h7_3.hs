import Control.Monad.State

-- Define the Random type alias using State Integer a
type Random a = State Integer a

-- Function to generate a fresh pseudo-random number
fresh :: Random Integer
fresh = do
  modify (\prev -> (6364136223846793005 * prev + 1442695040888963407) `mod` (2 ^ (64 :: Integer)))
  get

-- Function to run the pseudo-random number generator with an initial seed value
runPRNG :: Random a -> Integer -> a
runPRNG action seed = evalState action seed

main :: IO ()
main = do
  let seed = 42 -- Initial seed value
      result = runPRNG fresh seed
  putStrLn $ "Generated Random Number: " ++ show result
