import Today (getCurrentTimestamp, getDayFromTimestamp)

main :: IO ()
main = do
    timestamp <- getCurrentTimestamp
    putStrLn 
        (show $ getDayFromTimestamp timestamp)
