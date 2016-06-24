import Today (getCurrentTimestamp, getCurrentDay)

main :: IO ()
main = do
    timestamp <- getCurrentTimestamp
    putStrLn 
        (show $ getCurrentDay timestamp)
