import System.IO
import Control.Monad
import Data.List.Split
import Data.Char

checkHgt :: String -> Bool
checkHgt string = case typeval of
    "cm" -> value >= "150" && value <= "193"
    "in" -> value >= "59" && value <= "76" 
    _ -> False
    where
        typeval = [(string !! ((length string) - 2))] ++ [(string !! ((length string) - 1))]
        value = take ((length string) - 2) string

checkHcl :: String -> Bool
checkHcl value = ((value !! 0) == '#') && (all isAlphaNum (drop 1 value))

checkPid :: String -> Bool
checkPid value = (length value == 9) && (all isDigit value)

checkTerm :: String -> String -> Bool
checkTerm key value = case key of
    "byr" -> value >= "1920" && value <= "2002" && (length value == 4)
    "iyr" -> value >= "2010" && value <= "2020" && (length value == 4)
    "eyr" -> value >= "2020" && value <= "2030" && (length value == 4)
    "hgt" -> checkHgt value
    "hcl" -> checkHcl value
    "ecl" -> elem value ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    "pid" -> checkPid value
    _ -> True

keyValPairTest :: String -> Bool
keyValPairTest keyValPair = checkTerm key value where
    key = ((!! 0) . (splitOn ":")) keyValPair
    value = ((!! 1) . (splitOn ":")) keyValPair

testString :: String -> [String] -> Bool
testString string list = (go list splitString) && (all keyValPairTest (words string)) where
    go [] _ = True
    go _ [] = False
    go (x:xs) list = case (elem x list) of
        True -> go xs list
        False -> False
    splitString = helper (words string) where
        helper terms = map ((!! 0) . (splitOn ":")) terms

countPos :: [String] -> [String] -> Int
countPos list mains = go 0 list where
    go count [] = count
    go count (x:xs)
        | testString x mains = go (count + 1) xs
        | otherwise = go count xs

main = do
    handle <- openFile "4.txt" ReadMode
    contents <- hGetContents handle
    let list = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    print (countPos (splitOn "\n\n" contents) list)
    hClose handle