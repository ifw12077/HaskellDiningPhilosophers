module Main where

import Help         (prompt)
import Forks        (newFork)
import Seats        (newSeat)
import Philosophers (runPhilosopher, startPhilosophers)

main :: IO String
main = do
    numberOfPhilosophers <- prompt "How many philosophers?"

    numberOfSeats <- prompt "How many seats?"

    forks <- mapM newFork [1..numberOfSeats]

    let forkPairs = zip forks $ tail . cycle $ forks

    seats <- mapM newSeat $ zip [1..numberOfSeats] forkPairs

    let namedPhilosophers       = map (("Philosopher " ++) . show) [1..numberOfPhilosophers]
        philosophers            = map runPhilosopher namedPhilosophers
        table                   = map (:[]) seats
        philosophersAtTable     = zipWith ($) philosophers table

    putStrLn "Running the philosophers. Press enter to quit."

    startPhilosophers philosophersAtTable

    -- All threads exit when the main thread exits.
    getLine