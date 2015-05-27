module Seats (Seat, newSeat, releaseSeat, getSeat) where

import Control.Concurrent.STM   (TMVar, STM, newTMVarIO, takeTMVar, tryTakeTMVar, putTMVar, atomically)
import System.Random            (randomRIO)
import Forks                    (Forkpair)

type Seat = TMVar (Int, Forkpair)

newSeat :: (Int, Forkpair) -> IO Seat
newSeat = newTMVarIO

-- The basic transactional operations on seats
tryTakeSeat :: Seat -> STM (Maybe (Int, Forkpair))
tryTakeSeat = tryTakeTMVar

takeSeat :: Seat -> STM (Int, Forkpair)
takeSeat = takeTMVar

releaseSeat :: [(Int, Seat)] -> (Int, Forkpair) -> IO ()
releaseSeat (seat:seats) (number, forks)
    | number == fst seat    = atomically $ putTMVar (snd seat) (number, forks)
    | otherwise             = releaseSeat seats (number, forks)

getSeat :: [(Int, Seat)] -> IO (Int, Forkpair)
getSeat seats = do
    seat <- findSeat seats
    case seat of
        Nothing -> do
            mySeatNumber <- randomRIO (1, length seats)
            (place, forks) <- reserveSeat mySeatNumber seats
            return (place, forks)
        Just (place, forks) -> return (place, forks)

findSeat :: [(Int, Seat)] -> IO (Maybe (Int, Forkpair))
findSeat [] = return Nothing
findSeat (seat:seats) = do
    seat' <- atomically $ tryTakeSeat $ snd seat
    case seat' of
        Nothing -> findSeat seats
        Just (place, nSeat) -> return $ Just (place, nSeat)

reserveSeat :: Int -> [(Int, Seat)] -> IO (Int, Forkpair)
reserveSeat number (seat:seats)
    | number == fst seat    = atomically $ takeSeat $ snd seat
    | null seats            = atomically $ takeSeat $ snd seat
    | otherwise             = reserveSeat number seats