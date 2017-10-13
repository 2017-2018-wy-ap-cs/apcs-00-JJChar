

main :: IO()
main = do

  let
  event_loop board 1

event_loop :: [[Int]] -> Int -> IO()
event_loop board player = do
  putStrLn $ dB board

  if is_won board then do
    win_message board player
    return ()
  else do
    col <- get_move player
    handle_move board player col

next_ player = 3 - player

get_move player = do
  putStrLn $ "(Enter -99 to quit.)"
  putStrLn $ "Player " ++(show player)++" moves."
  putStrLn $ "Column [0-" ++(show (row - 1))++ "]? "
  x <- getLine
  return (get_number x)

-- get_number returns -1 for any invalid input
get_number :: String -> Int
get_number colIn
    = case (reads colIn)::[(Int,String)] of
        [(colnum, "")] -> colnum
        _              -> -1
                       
handle_move board player col
    | col == -99              = goodbye
    | isLegal board col = event_loop new_board (next_ player)
    | otherwise = complain_and_restart
    where complain_and_restart = do
              putStrLn "ERROR: That is not a legal move."
              event_loop board player
          new_board = makeMove board col player
          goodbye = do putStrLn "You quit"

win_message board player = do
    putStrLn $ "The game is over"
    putStrLn $ "Player "++(show $ next_ player)++" won!"
    -- note: win computed at the start of the next turn
    -- so current player is the loser
             
           
arrRef :: [[a]] -> Int -> Int -> a 
arrRef arr a b = (arr !! a) !! b

arrRef2 :: [[a]] -> (Int,Int) -> a 
arrRef2 arr (a,b) = (arr !! a) !! b

dB :: [[Int]] -> String
dH :: [Int] -> String
dH (a:b)
  | a == 1 = "X " ++ dH b 
  | a == 2 = "O " ++ dH b 
  | a == 0 = "_ " ++ dH b 
  | otherwise = ""
dH [] = ""
dB (a:b) = (dH a) ++ "\n" ++ (dB b)
dB [] = ""

row :: Int
col :: Int
row = 8
col = 4

board :: [[Int]]
board = [ [ 0 | x <- [1..row] ] | x <- [1..col] ]

drawBoard :: [[Int]] -> IO ()
drawBoard l = putStrLn $ dB l

nInL :: Eq a => a -> [a] -> Int 
nInL a l = length [ x | x <- l , x == a ]

findEmpty :: [[Int]] -> Int -> (Int,Int)
findEmpty arr col = ((nInL 0 (arrCol arr col)) - 1, col)

listSet :: [a] -> a -> Int -> [a]
listSet l n i = (take i l) ++ [n] ++ (drop (i + 1) l)

arrSet :: [[a]] -> a -> (Int,Int) -> [[a]]
arrSet arr n (a,b) = listSet arr (listSet (arr !! a) n b) a

isLegal :: [[Int]] -> Int -> Bool
isLegal arr col
  | col < 0 || col >= row = False
  | otherwise =  fst (findEmpty arr col) >= 0

makeMove :: [[Int]] -> Int -> Int -> [[Int]]
makeMove arr col i = (arrSet arr i (findEmpty arr col))
  
arrCol :: [[Int]] -> Int -> [Int]
arrCol arr col  = [ x !! col | x <- arr ]

winH :: [Int] -> Int
winH (a:b:c:d:e)
  | a == b && b == c && c == d && d /= 0 = a 
  | otherwise = winH (b:c:d:e)
winH (a) = 0

winCol :: [[Int]] -> Int -> Int
winRow :: [[Int]] -> Int -> Int
winCol arr i
  | i == length arr = winDia arr 
  | winH (arrCol arr i) == 0 = winCol arr (i + 1)
  | otherwise = winH (arrCol arr i)
winRow arr i 
  | i == length arr = winCol arr 0
  | winH (arr !! i) == 0 = winRow arr (i + 1)
  | otherwise = winH (arr !! 1)
  
intOr :: Int -> Int -> Int
intOr a 0 = a 
intOr 0 a = a 

winDia :: [[Int]] -> Int 
winDia ((a:b:c:d:e):(f:g:h:i:j):(k:l:m:n:o):(p:q:r:s:t):u)
  | a == g && g == m && m == s && s /= 0 = s 
  | p == l && l == h && h == d && d /= 0 = d 
  | otherwise = intOr (winDia ((b:c:d:e):(g:h:i:j):(l:m:n:o):(q:r:s:t):u))
                      (winDia((f:g:h:i:j):(k:l:m:n:o):(p:q:r:s:t):u))
winDia _ = 0

is_won :: [[Int]] -> Bool
is_won arr = winRow arr 0 /= 0
