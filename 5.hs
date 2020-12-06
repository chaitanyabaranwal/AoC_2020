import System.IO
import Control.Monad
import Data.List

calcSeat :: String -> Int
calcSeat string = go 0 127 0 7 string where
    go row _ col _ [] = row * 8 + col
    go minRow maxRow minCol maxCol (x:xs) = case x of
        'F' -> go minRow (div (minRow + maxRow) 2) minCol maxCol xs
        'B' -> go ((div (minRow + maxRow) 2) + 1) maxRow minCol maxCol xs
        'L' -> go minRow maxRow minCol (div (minCol + maxCol) 2) xs
        'R' -> go minRow maxRow ((div (minCol + maxCol) 2) + 1) maxCol xs
        _ -> error "invalid char"

findHighestSeat :: [String] -> Int
findHighestSeat list = go (-1) list where
    go max [] = max
    go max (x:xs)
        | calcSeat x > max = go (calcSeat x) xs
        | otherwise = go max xs

getSeatIDs :: [String] -> [Int]
getSeatIDs list = sort (map calcSeat list)

findSeatID :: [Int] -> Maybe Int
findSeatID seats = go 1 where
    go index
        | index == length seats = Nothing
        | (seats !! index) - (seats !! (index - 1)) == 2 = Just ((seats !! index) - 1)
        | otherwise = go (index + 1)

main = do
    handle <- openFile "5.txt" ReadMode
    contents <- hGetContents handle
    let list = words contents
    print (findSeatID (getSeatIDs list))
    hClose handle
