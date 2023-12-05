import Data.List
import Graphics.Win32 (aNSI_CHARSET)

{-
-- Vorlesungscode

main :: IO ()
main = do
    putStrLn "Eingabedatei"
    inname <- getLine
    infile <- readFile inname
    putStrLn "Ausgabedatei"
    outname <- getLine
    let outfile = unlines $ sort $ lines infile
    writeFile outname outfile
-}

-- Aufgabe 1: Zahlenspiel
guessNumber::Int -> Int -> Int -> IO Int
guessNumber lower upper attempts = do 
    let guess = (div (if even (upper - lower) then upper - lower else (upper - lower + 1)) 2) + lower - 1
    putStrLn $ "Is your number higher, lower or equal to " ++ show guess ++ "?"
    response <- getLine
    if response == "greater" then guessNumber (guess + 1) upper (attempts + 1)
    else if response == "smaller" then guessNumber lower (guess -1) (attempts + 1)
    else if response == "Yes" then return attempts
    else guessNumber lower upper attempts

main :: IO ()
main = do
    putStrLn "Choose a Number between 1 and 100"
    amountOfAttempts <- guessNumber 1 100 0
    putStrLn $ "I won in " ++ show amountOfAttempts ++ " attempts!"

