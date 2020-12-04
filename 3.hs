import System.IO
import Control.Monad

countTrees :: [String] -> Int -> Int -> Int
countTrees mapRows rightJump downJump = go 0 downJump rightJump where
    go count curRow curCol
        | curRow >= length mapRows = count
        | otherwise = case mapRows !! curRow !! curCol of
            '.' -> go count (curRow + downJump) (mod (curCol + rightJump) (length (mapRows !! (curRow + downJump))))
            '#' -> go (count + 1) (curRow + downJump) (mod (curCol + rightJump) (length (mapRows !! (curRow + downJump))))

main = do
    handle <- openFile "3.txt" ReadMode
    contents <- hGetContents handle
    let rows = words contents
    print ((countTrees rows 1 1) * (countTrees rows 3 1) * (countTrees rows 5 1) * (countTrees rows 7 1) * (countTrees rows 1 2))
    hClose handle
