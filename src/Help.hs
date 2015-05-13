module Help (prompt, unite) where

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

unite :: (Name -> [Seat] -> IO ()) -> [Name] -> [Seat] -> [IO()]
unite runphil philos seats
    | length phils == 0 = []
    | length phils == 1 = [runphil phil seats]
    | otherwise = runphil phil seats : unite runphil phils seats
    where phil = head philos
          phils = tail philos