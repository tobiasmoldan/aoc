import Data.Bifunctor (bimap)
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromJust)
import Data.Set (fromList, intersection)

parsePaddedInt :: String -> Maybe (Int, Int)
parsePaddedInt s =
  let p = p' False s
   in if null $ s' p then Nothing else Just (read (s' p) :: Int, length p)
  where
    p' r [] = []
    p' r (x : xs)
      | isDigit x = x : p' True xs
      | isSpace x && not r = x : p' False xs
      | otherwise = []
    s' [] = []
    s' (x : xs) = if isSpace x then s' xs else x : xs

parseLine :: String -> ([Int], [Int])
parseLine s = (i1, i2)
  where
    s1 = drop 4 s
    (_, l1) = fromJust . parsePaddedInt $ s1
    s2 = drop (l1 + 1) s1
    il' [] = []
    il' s = case parsePaddedInt s of
      Just (i, l) -> (i, l) : il' (drop l s)
      Nothing -> []
    il :: String -> ([Int], Int)
    il = foldr (\t v -> bimap (fst t :) (snd t +) v) ([], 0) . il'
    sp (x : xs)
      | isSpace x = 1 + sp xs
      | x == '|' = 1
      | otherwise = 0
    (i1, l2) = il s2
    s3 = drop l2 s2
    s4 = drop (sp s3) s3
    (i2, _) = il s4

wins :: ([Int], [Int]) -> Int
wins (l1, l2) = length $ intersection (fromList l1) (fromList l2)

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] l2 = l2
merge l1 [] = l1
merge (x1 : xs1) (x2 : xs2) = x1 + x2 : merge xs1 xs2

logic :: [String] -> Int
logic = l' []
  where
    cw = wins . parseLine
    l' :: [Int] -> [String] -> Int
    l' _ [] = 0
    l' [] (x : xs) = 1 + l' (replicate (cw x) 1) xs
    l' (x1 : xs1) (x2 : xs2) = 1 + x1 + l' (merge xs1 $ replicate (cw x2) (1 + x1)) xs2

main :: IO ()
main = do
  -- content <- readFile "input/demo_04.txt"
  content <- readFile "input/04.txt"
  print . logic . lines $ content