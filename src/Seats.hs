module Seats (Seat, newSeat, takeSeat, releaseSeat) where

import Control.Concurrent.STM   (TMVar, STM, newTMVarIO, takeTMVar, putTMVar)
import Forks                    (Forkpair)

type Seat = TMVar (Int, Forkpair)

newSeat :: (Int, Forkpair) -> IO Seat
newSeat (i, forks) = newTMVarIO (i, forks)

-- The basic transactional operations on forks
takeSeat :: Seat -> STM (Int, Forkpair)
takeSeat seat = takeTMVar seat

releaseSeat :: (Int, Forkpair) -> Seat -> STM ()
releaseSeat i seat = putTMVar seat i