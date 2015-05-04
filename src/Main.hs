module Main where

import Control.Concurrent
import Forks
import Philosophers

philosophers :: [String]
philosophers = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]

main :: IO String
main = do
    forks <- mapM newFork [1..5]

    let namedPhilosophers  = map runPhilosopher philosophers
        forkPairs          = zip forks (tail . cycle $ forks)
        philosophersWithForks = zipWith ($) namedPhilosophers forkPairs

    putStrLn "Running the philosophers. Press enter to quit."

    mapM_ forkIO philosophersWithForks
    -- All threads exit when the main thread exits.
    getLine