import System.IO
import Control.Monad
import Data.List
import Data.List.Split

countGroup :: String -> Int
countGroup string = (length . nub . join . words) string

countGroupIntersect :: String -> Int
countGroupIntersect string = length (foldr intersect (head (words string)) (words string))

main = do
    handle <- openFile "6.txt" ReadMode
    contents <- hGetContents handle
    let list = words contents
    print (sum (map countGroupIntersect (splitOn "\n\n" contents)))
    hClose handle