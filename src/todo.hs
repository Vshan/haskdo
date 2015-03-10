-- Todo.hs is a powerful tool to log, and sync mobile with web.
-- Sync Folder -> Logic Code -> Sync Folder -> Phone -> GNote ->
-- Sync Folder


-- Assume existence of three files which are as follows:
-- toweek.txt, totoday.txt, tolog.txt

{--
 - Here's how tolog.txt would look like (an example day):
 0730 chilling :z
 0740 bathe :h
 0750 leave for class :e
 0800 class begin :c
 0850 break :z
 0900 next class :c
 .
 .
 .
 0200 sleep
--}

{-- Directory Structure
 - data/
   -- Goals/
      -- DailyGoals
      -- WeeklyGoals
      -- MonthlyGoals
   -- RAW/
      -- - Should be of format 1__23-12-2014.txt,
                               2__24-12-2014.txt,
                               .
                               .
                               .
                               100__01-03-2015.txt
   -- Reports/
      -- - Same format as above
      -- WeeklyReports
      -- MonthlyReports
--}

{-- Reports Structure
 - The report for $DATE in $NAME's life.
 - Study: 340 minutes or 30% of total time.
 - Code: 453 minutes or 43% of total time.
   .
   .
   .
   .
 - Sleep: 2000 minutes or 20% of total time.
--}

{-- Here's how the functionality works:

  - The user logs his entire day in Google Keep on his phone.
  - The user also has a goal file where he records his goals for the day.

  - The log and goal file are exported to Dropbox, where it syncs with
  - user's other Dropbox folders.

  - On user's computer, a bash script, which runs periodically, syncs
  - the files from Dropbox to a local directory, which contains the
  - Haskell script.

  - The bash script starts on boot-up or wake-up and then runs every
  - 5 min until a control flag is set to true. Effectively saying that
  - sync has been succesful.

  - When control flag is true, the bash script runs the Haskell script
  - with the day's files and Haskell generates a master file $DAY.txt

  - Possibly through another Haskell script, generate an API which allows
  - two date arguments to be passed and generates some stats.

--}

{-- What's the end goal?

  - Create a program api which takes two (already logged) days
  - and calculates the cumulative time spent on each task along
  - with percentage of time spent.

  - Every day the program should generate a final $DAY.txt file
  - which has the logged activities, time spent + percentage and
  - the goal_of_today part.

--}

import System.IO
import System.Directory
import System.Environment
import Data.List
import Data.Time
import Data.Char
import Numeric

type Etime = (Int, Int)

data Etype = Study | Code | Read | Write | Hygiene | Exercise | Class
             | ClassWork | Food | Fun | Sleep deriving (Show, Eq)

data Event = Event { desc :: String,
                     etimeBegin :: Etime,
                     etimeEnd :: Etime,
                     etype :: Etype }

main :: IO ()
main = do
  args <- getArgs
  delegate args

delegate :: [String] -> IO ()
delegate [] = dailyScript
delegate [f, t] = genRepoFromTo f t

dailyScript :: IO ()
dailyScript = do
  logcontents <- readFile "../data/tolog.txt"
  gtocontents <- readFile "../data/goaltoday.txt"
  count <- readFile "../data/count.txt"
  tDate <- getDate
  let events = fmap parse (lines logcontents)
      repo = reportGen events
  writeFile ("../data/Goals/Daily/" ++ tDate ++ ".txt") gtocontents
  writeFile ("../data/RAW/" ++ count ++ "__" ++ tDate ++ ".txt") logcontents
  writeFile ("../data/Reports/Daily/" ++ tDate ++ ".txt") repo
  writeFile ("../data/count.txt") (show ((read count :: Int) + 1))

getEvents :: String -> String -> IO [Event]
getEvents f t = do
  dircon <- getDirectoryContents "./data/RAW"
  let min = toNum $ concat $ filter (isInfixOf f) dircon
      max = toNum $ concat $ filter (isInfixOf t) dircon
      fileNames = init $ tail $ dircon
      dfileNames = filter (\x -> (toNum x) `elem` [min..max]) fileNames
  allevents <- mapM readFile dfileNames
  return $ fmap (parse) (lines . concat $ allevents)

genRepoFromTo :: String -> String -> IO ()
genRepoFromTo f t = do
  events <- getEvents f t
  let report = reportGen events
  writeFile ("../data/Reports/From " ++ f ++ " to " ++ t ++ ".txt") report

getDate :: IO String
getDate = date >>= (\(x,y,z) ->
          return $ (show z) ++ "-" ++ (show y) ++ "-" ++ (show x))

date :: IO (Integer, Int, Int)
date = getCurrentTime >>= return . toGregorian . utctDay

toNum :: [Char] -> Int
toNum s = read (takeWhile (isDigit) s) :: Int

parse :: [Char] -> Event
parse e = Event {desc = (getDesc e),
                 etimeBegin = (getEtime (take 4 e)),
                 etimeEnd = (getEtime (take 4 $ drop 5 e)),
                 etype = (getEtype e)}

getDesc :: [Char] -> String
getDesc x =  reverse $ drop 3 $ reverse (drop 10 x)

getEtime :: [Char] -> Etime
getEtime [h1, h2, m1, m2] = (read [h1, h2] :: Int, read [m1, m2] :: Int)

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
                  "z:" -> Fun
                  "y:" -> Sleep

fmtFltN fn nod = showFFloat (Just nod) fn ""
disFl f = fmtFltN f 2

getTimeSpentinMin :: Etime -> Etime -> Int
getTimeSpentinMin (x1, y1) (x2, y2) = (x2 - x1)*60 + (y2 - y1)

allEtypes :: [Etype]
allEtypes = [Study, Code, Read, Write, Hygiene, Exercise, Class,
             ClassWork, Food, Fun, Sleep]

regen :: [Event] -> [(Int, Etype)]
regen es = fmap (\x -> ((timeSpentOn x es), x)) allEtypes

regenper :: [(Int, Etype)] -> [(Int, Etype, Float)]
regenper es = let th = sum $ fmap (\(x,y) -> x) es
              in fmap (\(x,y) ->
              (x,y,((fromIntegral x)*100 / (fromIntegral th)))) es

repogen :: [(Int, Etype, Float)] -> String
repogen es = concat $ fmap (\(x,y,z) ->
             (show y) ++ " : " ++ (show x) ++
             " minutes, or " ++ (disFl ((fromIntegral x) / 60)) ++
             " hours, or " ++ (disFl z) ++ "%\n") es

reportGen :: [Event] -> String
reportGen = repogen . regenper . regen

isOfType :: Event -> Etype -> Bool
x `isOfType` t = (etype x == t)

timeSpentOn :: Etype -> [Event] -> Int
timeSpentOn x es = sum
                   $ map (\v -> getTimeSpentinMin (etimeBegin v) (etimeEnd v))
                   $ filter (\b -> b `isOfType` x) es
