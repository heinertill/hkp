import Data.Char
import Data.List

newtype Stack a = Stack [a] deriving (Eq, Show)

pop :: Stack a -> Stack a
pop (Stack (_:xs)) = Stack xs
pop (Stack []) = error "Empty stack"

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

dup :: Stack Integer -> Stack Integer
dup (Stack (x:xs)) = push (x * 2) (Stack xs)
dup (Stack []) = error "Empty stack"

add :: Stack Integer -> Stack Integer
add (Stack (x:y:xs)) = push (x + y) (pop (pop (Stack xs)))
add (Stack _) = error "Not enough elements for addition"

sub :: Stack Integer -> Stack Integer
sub (Stack (x:y:xs)) = push (y - x) (pop (pop (Stack xs)))
sub (Stack _) = error "Not enough elements for subtraction"

multiply :: Stack Integer -> Stack Integer
multiply (Stack (x:y:xs)) = push (x * y) (pop (pop (Stack xs)))
multiply (Stack _) = error "Not enough elements for multiplication"

neg :: Stack Integer -> Stack Integer
neg (Stack (x:xs)) = push (negate x) (Stack xs)
neg (Stack []) = error "Empty stack"

stack :: Stack Integer
stack = Stack [0, 0 ..]

main :: IO ()
main = do
    putStrLn "Please choose an operation."
    operation <- getLine
    mainLoop operation stack

mainLoop :: String -> Stack Integer -> IO ()
mainLoop operation currentStack = do
    case operation of
        "exit" -> exit
        "push" -> do
            putStrLn "Enter a number to push onto the stack:"
            input <- getLine
            let num = read input :: Integer
            let newStack = push num currentStack
            putStrLn $ "Current Stack: " ++ show (take 2 (toList newStack))
            nextOperation <- ask operation
            mainLoop nextOperation newStack
        "pop" -> do
            let newStack = pop currentStack
            putStrLn $ "Current Stack: " ++ show (take 2 (toList newStack))
            nextOperation <- ask operation
            mainLoop nextOperation newStack
        "dup" -> do
            let newStack = dup currentStack
            putStrLn $ "Current Stack: " ++ show (take 2 (toList newStack))
            nextOperation <- ask operation
            mainLoop nextOperation newStack
        "add" -> do
            let newStack = add currentStack
            putStrLn $ "Current Stack: " ++ show (take 2 (toList newStack))
            nextOperation <- ask operation
            mainLoop nextOperation newStack
        "sub" -> do
            let newStack = sub currentStack
            putStrLn $ "Current Stack: " ++ show (take 2 (toList newStack))
            nextOperation <- ask operation
            mainLoop nextOperation newStack
        "multiply" -> do
            let newStack = multiply currentStack
            putStrLn $ "Current Stack: " ++ show (take 2 (toList newStack))
            nextOperation <- ask operation
            mainLoop nextOperation newStack
        _ -> do
            let newStack = neg currentStack
            putStrLn $ "Current Stack: " ++ show (take 2 (toList newStack))
            nextOperation <- ask operation
            mainLoop nextOperation newStack

ask :: String -> IO String
ask prevOperation = do
    putStrLn "Please choose an operation."
    getLine

exit :: IO ()
exit = putStrLn "Exiting program."

toList :: Stack a -> [a]
toList (Stack xs) = xs
