main = do putStrLn "Loaded"
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
board :: [[Int]]
board = [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
drawBoard :: [[Int]] -> IO ()
drawBoard l = putStrLn $ dB l
fe :: [[Int]] -> Int -> Int -> (Int,Int)
fe arr col i 
  | i == (length arr) = (i - 1,col)
  | arrRef arr i col /= 0 = (i - 1,col)
  | otherwise = fe arr col (i + 1)
findEmpty :: [[Int]] -> Int -> (Int,Int)
findEmpty arr col = fe arr col 0
listSet :: [a] -> a -> Int -> [a]
listSet l n i = (take i l) ++ [n] ++ (drop (i + 1) l)
arrSet :: [[a]] -> a -> (Int,Int) -> [[a]]
arrSet arr n (a,b) = listSet arr (listSet (arr !! a) n b) a
isLegal :: [[Int]] -> Int -> Bool
isLegal arr col = fst (findEmpty arr col) >= 0
makeMove :: [[Int]] -> Int -> Int -> [[Int]]
makeMove arr col i 
  | not $ isLegal arr col = arr
  | otherwise = (arrSet arr i (findEmpty arr col))
arrCol :: [[Int]] -> Int -> [Int]
arrCol arr col  = [ x !! col | x <- arr ]
intOr :: Int -> Int -> Int
intOr a b
  | a == 0 && b == 0 = 0 
  | otherwise = a 
win((a:b:c:d:e):(f:g:h:i:j):(k:l:m:n:o):(p:q:r:s:t):u)
  | a == f && f == k && k == p && p /= 0 = p 
  | a == b && b == c && c == d && d /= 0 = d 
  | a == g && g == m && m == s && s /= 0 = s 
  | d == h && h == l && l == p && p /= 0 = p 
  | otherwise = intOr (win ((b:c:d:e):(g:h:i:j):(l:m:n:o):u))
                      (win ((f:g:h:i:j):(k:l:m:n:o):(p:q:r:s:t):u))
win _ = 0