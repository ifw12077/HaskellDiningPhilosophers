module Main where

import Help         (prompt)
import Forks        (newFork)
import Seats        (newSeat)
import Philosophers (runPhilosopher, startPhilosophers)

main :: IO String
main = do
    numberOfPhilosophers <- prompt "How many philosophers?"

    numberOfSeats <- prompt "How many seats?"

    let philosophers = map (("Philosopher " ++) . show) [1..numberOfPhilosophers]

    forks <- mapM newFork [1..numberOfSeats]

    let forkPairs = zip forks $ tail . cycle $ forks

    seats <- mapM newSeat $ zip [1..numberOfSeats] forkPairs

    let namedPhilosophers  = map runPhilosopher philosophers

    putStrLn "Running the philosophers. Press enter to quit."

    startPhilosophers namedPhilosophers

    -- All threads exit when the main thread exits.
    getLine