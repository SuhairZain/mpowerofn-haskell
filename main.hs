import           System.Environment

isProductOf :: Int -> Int -> Int
isProductOf m n
  | m == n                                              = 1
  | m == 1                                              = 0
  | n == 1                                              = -1
  | m == 0                                              = -1
  | (m `mod` n == 0) && isProductOf (m `div` n) n >= 1  = 1 + isProductOf (m `div` n) n
  | otherwise                                           = -1

intStringPairsToInts :: [(Int, String)] -> [Int]
intStringPairsToInts pairs
  | null pairs = []
  | otherwise = intStringPairsToInts (reads (snd (head pairs))) ++ [fst (head pairs)]

stringsToInts :: [String] -> [Int]
stringsToInts strs
  | null strs = []
  | otherwise = intStringPairsToInts (reads (head strs)) ++ stringsToInts (tail strs)

first :: [Int] -> Int
first (x:_) = x
first _     = 1

second :: [Int] -> Int
second (_:x:_) = x
second _       = 1

main :: IO ()
main = do
  numsInput <- getArgs
  let (nums, m, n) = (stringsToInts numsInput, first nums, second nums) in print $ m `isProductOf` n
