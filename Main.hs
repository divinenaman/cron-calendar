
-- mins(0-59) hour(0-23) day-of-the-month(1-31) month(1-12) day-of-the-week(0-6)

data Cron = Cron {  minutes :: Int, 
                    hours :: Int,  
                    dom :: Int,
                    month :: Int,
                    dow :: Int
                }

instance Show Cron where
    show Cron { minutes = mins, hours = hr } = show mins ++  " " ++ show hr


main :: IO()
main = do
    putStrLn $ show Cron {minutes=5, hours=2, dom=4, month=3, dow=3}