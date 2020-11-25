module Tutorial9 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck


type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank d = d == ' '

-- 1.
group :: [a] -> [[a]]
group [] = []
group (x:y:s:xs) = [x,y,s] : group xs

groupBy :: Int -> [a] -> [[a]]
groupBy a xs | length xs >= a = ( take a xs ) : groupBy a (drop a xs)
             | otherwise = [xs]

-- 2.
intersperse :: a -> [a] -> [a]
intersperse z [] = [z]
intersperse z (x:xs) = z : x : intersperse z (xs) 

-- 3.
showRow :: String -> String
showRow xs = concat(intersperse "|" (group xs))

-- 4.
showGrid :: Matrix Digit -> [String]
showGrid [] = ["-------------"]
showGrid xs =  concat ( intersperse ["-------------"] (group xs))

{- 
Used showGrid (x:y:z:xs) = "-------------" : [x,y,z] ++ showGrid xz
whcih doesn't use the new functions so i decided to use one that does
-}
-- 5.
put :: Matrix Digit -> IO ()
put xs = putStrLn (unlines (showGrid [ showRow n | n <- xs ]))

-- 6.

helper [] = []
helper (x:xs) = x ++ helper xs

showMat :: Matrix Digit -> String
showMat (xs) = [ if n == ' ' then '.' else n | n <- helper xs] 

--readMat :: String -> Matrix Digit
readMat xs = groupBy 9 (proper xs)
             where proper xs = [ if n == '.' then ' ' else n | n <- xs] 

-- 7.
choices :: Matrix Digit -> Matrix [Digit]
choices [] = []
choices xs = [ n | n <- step , n /= []]
             where step = groupBy 9 [if n=="." then "123456789" else n | n <- groupBy 1 (showMat xs) , n /= ""] 

-- 8.
cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss ]

prop_cp :: [[a]] -> Bool
prop_cp xss = length (cp xss) == product (map length xss)
-- slow: use `quickCheckWith (stdArgs {maxSize = 5})`

expand :: Matrix [Digit] -> [Matrix Digit]
expand [] = []
expand xs = cp [ cp n | n <- xs]

-- 9.
prop_expand :: Matrix [Digit] -> Bool
prop_expand (xs) | null (expand xs) == False = length (head(expand xs)) == length (xs)
                 | otherwise = True


-- 10.
easySols :: Integer
easySols = product[fromIntegral(length n) | n <- concat(choices easy)]

--using expand takes too long so get length of each element of each list of lists and multiply

-- 11, 12, 13.
rows, cols, boxs :: Matrix a -> Matrix a
rows xs = xs
cols xs = transpose xs            
boxs xs = [ n | n <- concat(transpose(groupBy 3 (groupBy 9 (concat(concat(transpose(groupBy 3 (groupBy 3 (concat xs))))))))) , null n == False]
  where
    ungroup :: Matrix a -> [a]
    ungroup  = concat 

{- 
for cols i did not read we could use transpose till after completeing it so I ended up with this...
cols (x:xs) = take 9 (proxy (x:xs)) 
              where proxy (x:xs) = [ n | (t,n) <-zip [0..80] (concat (x:xs)), (mod t 9) == 0 ] : proxy ((drop 1 x):xs) 
it was functional but obviousy too long compared to just using transpose
-}

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs = nub xs == xs

-- 15.
valid :: Matrix Digit -> Bool
valid g = rules (rows g) && rules (cols g) && rules (boxs g)

rules:: Matrix Digit -> Bool
rules xs = length [ n | n <- xs , distinct n, (length n) == 9 , spaceChecker n] == (length xs)
            where spaceChecker n = [ x | x <- n, x /= ' '] == n


-- 16.
simple :: Matrix Digit -> [Matrix Digit]
simple = filter valid . expand . choices

{- this function is not viable. As tested with question 19 , expand (choices xs) where xs can be the sudoku puzzle easy*
will have an immense number of results which will take forever to compute.
-}


-- 17.
the :: [Digit] -> Digit
the [d] = d

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow [] = []
pruneRow (x:xs) = [ if length n == 1 then n else [ y | y <- n , elem y (errand (x:xs)) /= True] | n <- (x:xs)]

errand (x:xs) = concat [ n | n <- (x:xs) , length n == 1]


-- 18.
pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f  = f . map pruneRow . f


prune :: Matrix [Digit] -> Matrix [Digit]
prune xs = pruneBy boxs ( pruneBy cols (pruneBy rows xs))

-- 19.

many :: Eq a => (a -> a) -> a -> a
many g x | g x == g (g x) = g x
         | otherwise = many g (g x)  

{-
used to test many

close :: (Eq a, Ord a) => [(a,a)] -> [(a,a)]
close pairs = nub (sort (pairs ++[ (x,z) | (x,y) <- pairs,(y',z) <- pairs,y == y' ]))

-}

-- 20.
extract :: Matrix [Digit] -> Matrix Digit
extract xs = [ x | x <- groupBy 9 [ the n | n <- concat xs], x /= ""]


-- 21.
solve :: Matrix Digit -> Matrix Digit
solve xs = extract (many prune (choices xs)) 

-- it can solve book, easy and medium but not the other 2 :/


-- ** Optional Material

-- 22.
failed :: Matrix [Digit] -> Bool
failed mat = null [ n | n <- concat mat, n == " " ] == False

failed' mat = null [ n | n <- concat (many prune mat), n == " " ] == False

-- 23.
solved :: Matrix [Digit] -> Bool
solved xs = [ n | n <- concat xs, length n == 1] == concat xs

-- 24.
shortest :: Matrix [Digit] -> Int
shortest xs =  minimum [ length n | n <- (concat xs), length n >1]

-- 25.
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat = [ preMat ++ [preRow ++ [[n]] ++ postRow] ++ postMat | n <- ds]  
                 where (preMat, row:postMat) = break (\row -> null[ n | n <- row ,length n == shortest (mat)]==False) (mat)
                       (preRow, (ds):postRow) = break (\ds -> length ds == shortest (mat)) (row)

-- for use in expand1
fs = [["9","1578","3","67","167","4","2","15678","5678"],["4","178","6","5","12379","127","138","13789","789"],["57","157","2","8","13679","167","135","1345679","5679"],["238","238","9","267","2678","5","138","13678","4"],["58","6","7","1","4","3","9","2","58"],["1","23458","45","9","2678","267","358","35678","5678"],["2356","123459","145","246","1256","8","7","59","259"],["2567","1257","15","267","12567","9","4","58","3"],["257","24579","8","3","257","27","6","59","1"]]

{- 
list comprehension FTW!
-}

-- 26.
search :: Matrix Digit -> [Matrix Digit]
search xs | solved (many prune (choices xs)) = [solve xs]
          | failed (many prune (choices xs)) = error "No solutions"
          | otherwise = search' xs

search' xs = [(extract (many prune n)) | n <- helper xs, solved (many prune n)]
                 where helper xs = concat[(expand1 (many prune n)) | n <- expand1 (many prune (choices xs))]
                              
{- 
search works for hard not evil
-}

-- Example from Bird

book :: Matrix Digit
book = ["  4  57  ",
        "     94  ",
        "36      8",
        "72  6    ",
        "   4 2   ",
        "    8  93",
        "4      56", 
        "  53     ",
        "  61  9  "]

-- Examples from websudoku.com

easy :: Matrix Digit
easy = ["    345  ",
        "  89   3 ",
        "3    2789",
        "2 4  6815",
        "    4    ",
        "8765  4 2",
        "7523    6",
        " 1   79  ",
        "  942    "]

medium :: Matrix Digit
medium = ["   4 6 9 ",
          "     3  5",
          "45     86",
          "6 2 74  1",
          "    9    ",
          "9  56 7 8",
          "71     64",
          "3  6     ",
          " 6 9 2   "]

hard :: Matrix Digit
hard = ["9 3  42  ",
        "4 65     ",
        "  28     ",
        "     5  4",
        " 67 4 92 ",
        "1  9     ",
        "     87  ",
        "     94 3",
        "  83  6 1"]

evil :: Matrix Digit
evil = ["  9      ",
        "384   5  ",
        "    4 3  ",
        "   1  27 ",
        "2  3 4  5",
        " 48  6   ",
        "  6 1    ",
        "  7   629",
        "     5   "]

puts :: [Matrix Digit] -> IO ()
puts = sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (search g) >> putStrLn "***"
       
main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil

                        
