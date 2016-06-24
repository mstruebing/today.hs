module Today (Day(..), getCurrentTimestamp, getDayFromTimestamp) where
import Data.Time.Clock.POSIX(getPOSIXTime)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Show, Bounded, Enum)

-- Returns the current day
getDayFromTimestamp :: Integer -> Day
getDayFromTimestamp ts = getDayFromIndex . 
                            fromIntegral .
                            calculateDayIndex .
                            calculateElapsedDays $
                            ts

-- returns a day of a given index
getDayFromIndex :: Int -> Day
getDayFromIndex x
    | x < 0 || x > 6 = error "Index must be between 0 and 6"
    | otherwise = toEnum x::Day

-- returns the current timestamp
getCurrentTimestamp :: IO Integer
getCurrentTimestamp = (round `fmap` getPOSIXTime)

-- calculate the elapsed days since 01.01.1970
calculateElapsedDays:: Integer -> Integer
calculateElapsedDays ts = ts `div` 86400


-- add 3 beacuse the 01.01.1970 was a Thursday
calculateDayIndex :: Integer -> Integer
calculateDayIndex x = (x + 3) `mod` 7


