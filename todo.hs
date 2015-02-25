-- Todo.hs is a powerful tool to log, and sync mobile with web.
-- Sync Folder -> Logic Code -> Sync Folder -> Phone -> GNote ->
-- Sync Folder


-- Assume existence of three files which are as follows:
-- toweek.txt, totoday.txt, tolog.txt

{--
 - Here's how tolog.txt would look like (an example day):
 0730 chilling :z
 0740 crap :h
 0750 leave for class :e
 0800 class begin :c
 0850 break :z
 0900 next class :c
 .
 .
 .
 0200 sleep
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
import Control.Applicative
import Data.Time

type Etime = (Int, Int)

data Etype = Study | Code | Read | Write | Hygiene | Exercise | Class
             | ClassWork | Food | Zone | Sleep deriving Show

data Event = Event { desc :: String, etime :: Etime, etype :: Etype }

main :: IO ()
main = do
  logcontents <- readFile "tolog.txt"
  gtocontents <- readFile "goalday.txt"
  let events = parse <$> (lines logcontents)
      goals = lines gtocontents
  tdate <- getCurrentTime
  count <- readFile "count.txt"
  writeFile (count ++ " " ++ tdate ++ ".txt") (gento events goals)
  writeFile "count.txt" (show ((read count :: Int) + 1))



parse :: [Char] -> Event
parse e@(t1:t2:t3:t4:_) = Event {desc = (getDesc e),
                             etime = (getEtime $ t1 ++ t2 ++ t3 ++ t4),
                             etype = (getEtype e)}

getDesc :: [Char] -> String
getDesc x = case x of
              (_:_:_:_:_:d) -> case (reverse d) of
                                 (_:_:f) -> reverse f

getEtime :: [Char] -> Etime
getEtime (h1:h2:m1:m2) = (read (h1 ++ h2) :: Int, read (m1 ++ m2) :: Int)

getEtype :: [Char] -> Etype
getEtype x = case (reverse x) of
               (x1:x2:_) -> case (x1 ++ x2) of
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

gento :: [a] -> [b] -> String
