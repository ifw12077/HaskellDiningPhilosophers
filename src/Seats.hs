module Seats (Seat, newSeat, takeSeat, tryTakeSeat, releaseSeat) where

import Control.Concurrent.STM   (TMVar, STM, newTMVarIO, takeTMVar, tryTakeTMVar, tryPutTMVar)
import Forks                    (Forkpair)

type Seat = TMVar (Int, Forkpair)

newSeat :: (Int, Forkpair) -> IO Seat
newSeat (i, forks) = newTMVarIO (i, forks)

-- The basic transactional operations on seats
tryTakeSeat :: Seat -> STM (Maybe (Int, Forkpair))
tryTakeSeat seat = tryTakeTMVar seat

takeSeat :: Seat -> STM (Int, Forkpair)
takeSeat seat = takeTMVar seat

releaseSeat :: (Int, Forkpair) -> Seat -> STM Bool
releaseSeat i seat = tryPutTMVar seat i