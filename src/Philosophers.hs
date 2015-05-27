module Philosophers (Name, runPhilosopher, startPhilosophers) where

import Control.Monad            (forever)
import Control.Concurrent       (threadDelay, forkIO)
import Seats                    (Seat, getSeat, releaseSeat)
import Forks                    (getForks, releaseForks)

type Name = String

eatDelay :: Int
eatDelay = 500000

thinkDelay :: Int
thinkDelay = 1000000

sleepDelay :: Int
sleepDelay = 3000000

runPhilosopher :: Name -> [(Int, Seat)] -> IO ()
runPhilosopher name seats = forever $ do
    putStrLn (name ++ " is born.")
    startDay name seats 0 0

startPhilosophers :: [IO ()] -> IO ()
startPhilosophers = mapM_ forkIO

startDay :: Name -> [(Int, Seat)] -> Int -> Int -> IO ()
startDay name seats day days = do
    putStrLn (name ++ " is thinking.")
    threadDelay thinkDelay
    startEating name seats
    if (day + 1) > 3
    then do
        putStrLn (name ++ " finished day " ++ show days)
        threadDelay sleepDelay
        startDay name seats 0 (days + 1)
    else startDay name seats (day + 1) days

startEating :: Name -> [(Int, Seat)] -> IO ()
startEating name seats = do
    (place, forks) <- getSeat seats
    putStrLn (name ++ " got seat " ++ show place ++ ".")
    (left, right) <- getForks forks
    putStrLn (name ++ " got forks " ++ show left ++ " and " ++ show right ++ " and is now eating.")
    threadDelay eatDelay
    releaseForks forks (left, right)
    releaseSeat seats (place, forks)