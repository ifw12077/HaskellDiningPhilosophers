module Help (prompt, unite, set) where

import Data.Maybe   (listToMaybe)
import Seats        (Seat)
import Philosophers (Name)

prompt :: String -> IO Int
prompt x = do
    putStrLn x
    maybeInt <- fmap maybeRead getLine :: IO (Maybe Int)
    case maybeInt of
        Nothing -> do
            putStrLn "Invalid input!"
            prompt x
        Just y  -> return y

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads

unite :: (Name -> [(Int, Seat)] -> IO ()) -> [Name] -> [(Int, Seat)] -> [IO()]
unite runphil philos seats
    | null phils        = []
    | length phils == 1 = [runphil phil seats]
    | otherwise         = runphil phil seats : unite runphil phils seats
    where phil  = head philos
          phils = tail philos

set :: [Int] -> [Seat] -> [(Int, Seat)]
set places seats
    | null seats || null places                 = []
    | length seats /= length places             = []
    | length seats == 1 && length places == 1   = [(place, seat)]
    | otherwise                                 = (place, seat) : set places' seats'
    where place     = head places
          seat      = head seats
          places'   = tail places
          seats'    = tail seats