module Main where

import Help         (prompt, unite, set)
import Forks        (newFork)
import Seats        (newSeat)
import Philosophers (runPhilosopher, startPhilosophers)

main :: IO String
main = do
    numberOfPhilosophers    <- prompt "How many philosophers?"

    numberOfSeats           <- prompt "How many seats?"

    let nrSeats             = [1..numberOfSeats]

    forks                   <- mapM newFork nrSeats

    let forkPairs           = zip forks $ tail . cycle $ forks

    seats                   <- mapM newSeat $ zip nrSeats forkPairs

    let namedPhilosophers   = map (("Philosopher " ++) . show) [1..numberOfPhilosophers]
        table               = set nrSeats seats
        philosophers        = unite runPhilosopher namedPhilosophers table

    putStrLn "Running the philosophers. Press enter to quit."

    startPhilosophers philosophers

    -- All threads exit when the main thread exits.
    getLine