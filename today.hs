import Data.Time.Clock.POSIX(getPOSIXTime)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Bounded, Enum)

main :: IO ()
main = do
    timestamp <- getCurrentTimestamp
    putStrLn 
        (show 
            (getDayList !! (fromIntegral $ 
                            calculateCurrentDayIndex . 
                            calculateElapsedDays $ 
                            timestamp)))

-- create a list with all days in order
getDayList :: [Day]
getDayList = [minBound .. maxBound] :: [Day]

-- returns the current timestamp
getCurrentTimestamp :: IO Integer
getCurrentTimestamp = (round `fmap` getPOSIXTime)

-- calculate the elapsed days since 01.01.1970
calculateElapsedDays:: Integer-> Integer
calculateElapsedDays ts = ts `div` 86400


-- add 3 beacuse the 01.01.1970 was a Thursday
calculateCurrentDayIndex :: Integer-> Integer
calculateCurrentDayIndex x = (x + 3) `mod` 7


