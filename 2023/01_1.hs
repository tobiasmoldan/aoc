import Data.Char (digitToInt, isDigit)
import Data.List
import Data.String
import Text.Printf (printf)

extractNums :: String -> [Int]
extractNums xs = map digitToInt (filter isDigit xs)

buildNum :: [Int] -> Int
buildNum [] = 0
buildNum xs = 10 * head xs + last xs

parseLines :: [String] -> Int
parseLines xs = sum $ map (buildNum . extractNums) xs

main :: IO ()
main = do
  content <- readFile "input/01.txt"
  -- let content = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
  print $ parseLines $ lines content
