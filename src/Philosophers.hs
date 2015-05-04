module Philosophers where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Random
import Forks

type Name = String

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
    delay <- randomRIO (1,10)
    threadDelay (delay * 1000000) -- 1, 10 seconds. threadDelay uses nanoseconds.
    putStrLn (name ++ " is done eating. Going back to thinking.")

    atomically $ do
        releaseFork leftNum left
        releaseFork rightNum right

    delay2 <- randomRIO (1, 10)
    threadDelay (delay2 * 1000000)

startPhilosophers :: [IO ()] -> [IO ThreadId] --(IO () -> [ThreadId]) -> [IO ()] -> [()]
startPhilosophers = mapM_ forkIO