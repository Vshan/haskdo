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


{--
 - Here's how totoday.txt would look like:
 -
 -
 -
 -
 -
--}


{-- Here's how toweek.txt would look like:
 -
 -
 -
 -
 -
 -
--}

{-- What's the end goal?

  - Create a program api which takes two (already logged) days
  - and calculates the cumulative time spent on each task along
  - with percentage of time spent.

  - Every day the program should generate a final $DAY.txt file
  - which has the logged activities, time spent + percentage and
  - the goal_of_today part.

--}

type Etime = (Int, Int)

data Etype = Study | Code | Read | Write | Hygiene | Exercise | Class
             | ClassWork | Food | Zone deriving Show

data Event = Event { desc :: String, etime :: Etime, etype :: Etype }

main :: IO ()
main = do
  contents <- readFile "tolog.txt"
  let events = map (parse) (lines contents)

parse :: [Char] -> Event
parse e@(t1:t2:t3:t4:_) = Event {desc = (getDesc e),
                             etime = (getEtime (t1 ++ t2 ++ t3 ++ t4)),
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
