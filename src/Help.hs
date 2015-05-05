module Help (prompt) where

import Data.Maybe   (listToMaybe)

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