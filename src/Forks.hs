module Forks where

import Control.Concurrent.STM   (TMVar, STM, newTMVarIO, takeTMVar, putTMVar)

type Fork = TMVar Int

newFork :: Int -> IO Fork
newFork i = newTMVarIO i

-- The basic transactional operations on forks
takeFork :: Fork -> STM Int
takeFork fork = takeTMVar fork

releaseFork :: Int -> Fork -> STM ()
releaseFork i fork = putTMVar fork i