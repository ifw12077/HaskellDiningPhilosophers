module Main where

import Data.Maybe   (listToMaybe)
import Forks        (newFork)
import Philosophers (runPhilosopher, startPhilosophers)

philosophers :: [String]
philosophers = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]

prompt :: String -> IO Int
prompt x = do
    putStrLn x
    maybeInt <- fmap maybeRead getLine :: IO (Maybe Int)
    case maybeInt of
        Nothing -> do
            putStrLn "Invalid input!"
            prompt x
        Just y  -> return y

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads

main :: IO String
main = do
    numberOfPhilosophers <- prompt "How many philosophers?"
    numberOfSeats <- prompt "How many seats?"

    forks <- mapM newFork [1..numberOfSeats]

    let namedPhilosophers  = map runPhilosopher philosophers
        forkPairs          = zip forks (tail . cycle $ forks)
        philosophersWithForks = zipWith ($) namedPhilosophers forkPairs

    putStrLn "Running the philosophers. Press enter to quit."

    startPhilosophers philosophersWithForks

    -- All threads exit when the main thread exits.
    getLine