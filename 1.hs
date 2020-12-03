import qualified Data.IntSet as IntSet
import System.IO
import Control.Monad

toInt :: String -> Int
toInt = read

twoSum :: Int -> [Int] -> Maybe (Int, Int)
twoSum target items = go IntSet.empty items where
    go seen [] = Nothing
    go seen (cur:next) = case IntSet.member (target - cur) seen of
        True -> Just (cur, target - cur)
        False -> go (IntSet.insert cur seen) next

threeSum :: Int -> [Int] -> Maybe (Int, Int, Int)
threeSum target items = go items where
    go [] = Nothing
    go (cur:next) = case (twoSum (target - cur) next) of
        Just (x, y) -> Just (x, y, cur)
        Nothing -> go next

main = do
    let list = []
    handle <- openFile "1.txt" ReadMode
    contents <- hGetContents handle
    let singleWords = words contents
        nums = map toInt singleWords
    print (threeSum 2020 nums)
    hClose handle
