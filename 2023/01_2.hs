import Data.Char (digitToInt, isDigit)
import Data.Maybe (catMaybes)
import Data.String (lines)

scanPos :: [Char] -> Maybe Int
scanPos xxs =
  case xxs of
    ('o' : 'n' : 'e' : _) -> Just 1
    ('t' : 'w' : 'o' : _) -> Just 2
    ('t' : 'h' : 'r' : 'e' : 'e' : _) -> Just 3
    ('f' : 'o' : 'u' : 'r' : _) -> Just 4
    ('f' : 'i' : 'v' : 'e' : _) -> Just 5
    ('s' : 'i' : 'x' : _) -> Just 6
    ('s' : 'e' : 'v' : 'e' : 'n' : _) -> Just 7
    ('e' : 'i' : 'g' : 'h' : 't' : xs) -> Just 8
    ('n' : 'i' : 'n' : 'e' : _) -> Just 9
    (x : _) -> if isDigit x then Just $ digitToInt x else Nothing
    [] -> Nothing

scanLine' :: [Char] -> [Maybe Int]
scanLine' [] = []
scanLine' xxs = scanPos xxs : scanLine' (tail xxs)

scanLine :: [Char] -> [Int]
scanLine = catMaybes . scanLine'

buildNum :: [Int] -> Int
buildNum [] = 0
buildNum xs = 10 * head xs + last xs

parseLines :: [String] -> Int
parseLines lines =
  sum $ map (buildNum . scanLine) lines

main :: IO ()
main = do
  content <- readFile "input/01.txt"
  -- content <- readFile "input/demo_01.txt"
  -- let content = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
  print $ parseLines $ lines content
