module Philosophers (Name, runPhilosopher, startPhilosophers) where

import Control.Monad            (forever)
import Control.Concurrent       (threadDelay, forkIO)
import Data.List                (find)
import Data.Maybe               (isJust)
import System.Random            (randomRIO)
import Seats                    (Seat, tryTakeSeat, takeSeat, releaseSeat)

type Name = String

eatDelay :: Int
eatDelay = 5000000

thinkDelay :: Int
thinkDelay = 10000000

sleepDelay :: Int
sleepDelay = 30000000

runPhilosopher :: Name -> [Seat] -> IO ()
runPhilosopher name seats = forever $ do
    putStrLn (name ++ " is born.")
    startDay name seats 0 0

startPhilosophers :: [IO ()] -> IO ()
startPhilosophers = mapM_ forkIO

startDay :: Name -> [Seat] -> Int -> Int -> IO ()
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

startEating :: Name -> [Seat] -> IO ()
startEating name seats = do
    seat <- find (\x -> isJust (tryTakeSeat x)) seats
    case seat of
        Nothing     -> do
            mySeatNumber <- randomRIO (1, length seats)
            takeSeat 
        Just place  -> putStrLn ""