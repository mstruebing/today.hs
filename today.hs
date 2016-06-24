import Data.Time.Clock.POSIX(getPOSIXTime)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

main :: IO ()
main = do
    timestamp <- getCurrentTimestamp
    putStrLn 
        (show 
            (getDayList !! 
            (fromIntegral 
            (calculateCurrentDayIndex
            (calculateElapsedDays timestamp
            )))))


getDayList :: [Day]
getDayList = [minBound .. maxBound] :: [Day]

getCurrentTimestamp :: IO Integer
getCurrentTimestamp = (round `fmap` getPOSIXTime)

calculateElapsedDays:: Integer-> Integer
calculateElapsedDays ts = ts `div` 86400


-- add 3 beacuse the 1.1.1970 was a Thursday
calculateCurrentDayIndex :: Integer-> Integer
calculateCurrentDayIndex x = (x + 3) `mod` 7


