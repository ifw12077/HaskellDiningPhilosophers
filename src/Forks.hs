module Forks (Fork, Forkpair, newFork, getForks, releaseForks) where

import Control.Concurrent.STM   (TMVar, STM, newTMVarIO, takeTMVar, putTMVar, atomically)

type Fork = TMVar Int
type Forkpair = (Fork, Fork)

newFork :: Int -> IO Fork
newFork i = newTMVarIO i

-- The basic transactional operations on forks
takeFork :: Fork -> STM Int
takeFork fork = takeTMVar fork

releaseFork :: Int -> Fork -> STM ()
releaseFork i fork = putTMVar fork i

getForks :: Forkpair -> IO (Int,Int)
getForks (left, right) = do
    (leftNum, rightNum) <- atomically $ do
        leftNum <- takeFork left
        rightNum <- takeFork right
        return (leftNum, rightNum)
    return (leftNum, rightNum)

releaseForks :: Forkpair -> (Int, Int) -> IO ()
releaseForks (left, right) (leftNum, rightNum) = atomically $ do
    releaseFork leftNum left
    releaseFork rightNum right