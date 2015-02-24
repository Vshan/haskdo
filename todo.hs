-- Todo.hs is a powerful tool to log, and sync mobile with web.
-- Sync Folder -> Logic Code -> Sync Folder -> Phone -> GNote ->
-- Sync Folder


-- Assume existence of three files which are as follows:
-- toweek.txt, totoday.txt, tolog.txt

{--
 - Here's how tolog.txt would look like:
 0730 got up :z
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

data Event = Event { desc :: String,
                     time :: String,
                     etype :: String
                   }

main = do
  contents <- readFile "tolog.txt"
  let events = map (parse) (lines contents)

parse :: [Char] -> Event
