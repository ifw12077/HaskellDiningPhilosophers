module Main where

import Help         (prompt)
import Forks        (newFork)
import Philosophers (philosophers, runPhilosopher, startPhilosophers)

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