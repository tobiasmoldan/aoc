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

main :: IO ()
main = do
  -- content <- readFile "input/demo_04.txt"
  content <- readFile "input/04.txt"
  let ls = sum $ map ((2 ^) . (+ (-1))) $ filter (/= 0) $ map (length . uncurry intersection . bimap fromList fromList . parseLine) (lines content)
   in print ls