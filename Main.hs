import qualified Data.Time as Time

-- mins(0-59) hour(0-23) day-of-the-month(1-31) month(1-12) day-of-the-week(0-6)

data Month = January | Febuary | March | April | May | June | July | August | September | October | November | December deriving(Eq, Ord, Enum, Show)
data ReminderRecurrence = Daily | Weekly deriving(Show)


checkElementInList :: (Ord a) => a -> [a] -> Bool
checkElementInList _ [] = False 
checkElementInList ele (x:xs)
    | ele == x = True
    | otherwise = checkElementInList ele xs    

type Year = Integer
checkLeapYear :: Year -> Bool
checkLeapYear year
    | (mod year 4) == 0 && (mod year 100) == 0 && (mod year 400) == 0 = True 
    | otherwise = False

checkRange :: (Ord a) => a -> a -> a -> Bool
checkRange a b c = c >= a && c <= b 

type Day = Integer
checkDayOfMonthInYear :: Year -> Month -> Day -> Bool
checkDayOfMonthInYear year month day
    | checkElementInList month [January, March, May, July, August, October, December] && checkRange 1 31 day = True
    | checkElementInList month [April, June, September, November] && checkRange 1 30 day = True
    | month == Febuary && checkRange 1 (if leapYear then 29 else 28) day = True
    | otherwise = error "Invalid date" 
    where leapYear = checkLeapYear year

type Minutes = Integer
type Hour = Integer
checkTime :: Minutes -> Hour -> Bool
checkTime mins hrs
    | checkRange 0 59 mins && checkRange 0 23 hrs = True
    | otherwise = error "Invalid time"

type Datetime = (Year, Month, Day, Minutes, Hour)
datetime :: Year -> Month -> Day -> Minutes -> Hour -> Datetime 
datetime year month date mins hrs
    | isDateCorrect && isTimeCorrect = (year, month, date, mins, hrs) 
    | otherwise = error "Invalid datetime"
    where isDateCorrect = checkDayOfMonthInYear year month date
          isTimeCorrect = checkTime mins hrs

datetimeRange :: Datetime -> Datetime -> [Datetime]
datetimeRange d1 d2
    | d1 <= d2 = [d1, d2]
    | otherwise = error "Invalid datetime range"


offset:: (Ord a, Enum a) => Integer -> a -> a -> Integer 
offset off a b
    | a == b = off
    | b > a = offset (off + 1) (succ a) b
    | b < a = offset (off - 1) (pred a) b 

padLeftZero:: (Ord a, Show a) => a -> a -> String
padLeftZero b a
    | a < b = "0" ++ show a
    | otherwise = show a

padLeftZeroIfLessThanThen = padLeftZero 10

monthOffset a = offset 1 January a

datetimeString:: Datetime -> String
datetimeString (year, month, date, mins, hrs) = (padLeftZeroIfLessThanThen year) ++ "-" ++ (padLeftZeroIfLessThanThen $ monthOffset month) ++ "-" ++ (padLeftZeroIfLessThanThen date) ++ "T" ++ (padLeftZeroIfLessThanThen hrs) ++ ":" ++ (padLeftZeroIfLessThanThen mins)  

main :: IO()
main = do

    now <- Time.getCurrentTime
    timezone <- Time.getCurrentTimeZone

    let zoneNow = Time.utcToLocalTime timezone now
    let (year, _, _) =  Time.toGregorian $ Time.localDay zoneNow

    let localTimeoffset = Time.timeZoneOffsetString timezone
    
    let d1 = datetime year December 10 30 3
    let d2 = datetime year Febuary 10 30 3

    let drange = datetimeRange d1 d2

    putStrLn $ datetimeString d1    
    

