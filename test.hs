import Data.List

type Etime = (Int, Int)

data Etype = Study | Code | Read | Write | Hygiene | Exercise | Class
             | ClassWork | Food | Zone | Sleep deriving (Show, Eq)

data Event = Event { desc :: String,
                     etimeBegin :: Etime,
                     etimeEnd :: Etime,
                     etype :: Etype }

parse :: [Char] -> Event
parse e = Event {desc = (getDesc e),
                 etimeBegin = (getEtime (take 4 e)),
                 etimeEnd = (getEtime (take 4 $ drop 5 e)),
                 etype = (getEtype e)}

getDesc :: [Char] -> String
getDesc x =  reverse $ drop 3 $ reverse (drop 10 x)

getEtime :: [Char] -> Etime
getEtime [h1, h2, m1, m2] = (read [h1,h2] :: Int, read [m1,m2] :: Int)

getEtype :: [Char] -> Etype
getEtype x = case (take 2 $ reverse x) of
                  "s:" -> Study
                  "c:" -> Code
                  "r:" -> Read
                  "w:" -> Write
                  "h:" -> Hygiene
                  "e:" -> Exercise
                  "g:" -> Class
                  "b:" -> ClassWork
                  "f:" -> Food
                  "z:" -> Zone
                  "y:" -> Sleep

getTimeSpentinMin :: Etime -> Etime -> Int
getTimeSpentinMin (x1, y1) (x2, y2) = (x2 - x1)*60 + (y2 - y1)

isOfType :: Event -> Etype -> Bool
x `isOfType` t = (etype x == t)

timeSpentOn :: Etype -> [Event] -> Int
timeSpentOn x es = sum
                   $ map (\v -> getTimeSpentinMin (etimeBegin v) (etimeEnd v))
                   $ filter (\b -> b `isOfType` x) es
