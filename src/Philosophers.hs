module Philosophers (philosophers, runPhilosopher, startPhilosophers) where

import Control.Monad            (forever)
import Control.Concurrent       (threadDelay, forkIO)
import Control.Concurrent.STM   (atomically)
import Forks                    (Fork, takeFork, releaseFork)

type Name = String

philosophers :: [String]
philosophers = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]

eatDelay :: Int
eatDelay = 5000000

thinkDelay :: Int
thinkDelay = 10000000

sleepDelay :: Int
sleepDelay = 30000000

runPhilosopher :: Name -> (Fork, Fork) -> IO ()
runPhilosopher name (left, right) = forever $ do
    putStrLn (name ++ " is hungry.")

    -- Run the transactional action atomically.
    -- The type system ensures this is the only way to run transactional actions.
    (leftNum, rightNum) <- atomically $ do
        leftNum <- takeFork left
        rightNum <- takeFork right
        return (leftNum, rightNum)

    putStrLn (name ++ " got forks " ++ show leftNum ++ " and " ++ show rightNum ++ " and is now eating.")
    threadDelay eatDelay
    putStrLn (name ++ " is done eating. Going back to thinking.")

    atomically $ do
        releaseFork leftNum left
        releaseFork rightNum right

    threadDelay sleepDelay

startPhilosophers :: [IO ()] -> IO ()
startPhilosophers = mapM_ forkIO