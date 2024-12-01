import Control.Exception (throw)
import Data.Char (isDigit, isSpace)
import Data.Int (Int64)
import Data.Map qualified as M (Map, fromList, lookup)
import Data.Maybe (catMaybes, fromJust)

data GenMap = GenMap {from :: String, to :: String, mapping :: [(Int64, Int64, Int64)]} deriving (Show)

data Almanac = Almanac {seeds :: [Int64], mappings :: M.Map String GenMap} deriving (Show)

parseInt :: String -> Maybe (Int64, Int)
parseInt s =
  let p = p' False s
   in if null $ s' p then Nothing else Just (read (s' p) :: Int64, length p)
  where
    p' r [] = []
    p' r (x : xs)
      | isDigit x = x : p' True xs
      | isSpace x && not r = x : p' False xs
      | otherwise = []
    s' [] = []
    s' (x : xs) = if isSpace x then s' xs else x : xs

isBlank :: String -> Bool
isBlank = foldr ((&&) . isSpace) True

parseSeeds :: String -> [Int64]
parseSeeds s = g seeds
  where
    p s = case parseInt s of
      Nothing -> []
      Just (i, l) -> i : p (drop l s)
    seeds = p . drop 6 $ s
    g [] = []
    g (x : r : xs) = g' x r ++ g xs
      where
        g' x 0 = [x]
        g' x r = x : g' (x + 1) (r - 1)

-- is to be run after skipping the seeds section
splitSections :: [String] -> [[String]]
splitSections = filter (not . null) . s
  where
    s' [] = []
    s' (x : xs)
      | isBlank x = []
      | otherwise = x : s' xs
    s [] = []
    s l = x : s (drop (len + 1) l)
      where
        x = s' l
        len = length x

parseSection :: [String] -> GenMap
parseSection (x : xs) = GenMap from to $ m xs
  where
    t [] = []
    t (x : xs) = if isSpace x || x == '-' then [] else x : t xs
    p s = (i2, i1, i3)
      where
        (i1, l1) = fromJust . parseInt $ s
        s1 = drop l1 s
        (i2, l2) = fromJust . parseInt $ s1
        (i3, _) = fromJust . parseInt $ drop l2 s1
    from = t x
    x2 = drop (4 + length from) x
    to = t x2
    m [] = []
    m (x : xs) = p x : m xs

parseAlmanac :: String -> Almanac
parseAlmanac s = Almanac (parseSeeds rseeds) (M.fromList $ map ((\s -> (from s, s)) . parseSection) $ splitSections rsections)
  where
    (rseeds : rsections) = lines s

getLowestLocation :: Almanac -> Int64
getLowestLocation a = minimum $ l (seeds a)
  where
    l'' :: Int64 -> [(Int64, Int64, Int64)] -> Int64
    l'' v [] = v
    l'' v ((f, t, r) : xs) = if v >= f && v < (f + r) then t + v - f else l'' v xs
    l' :: String -> Int64 -> Int64
    l' "location" x = x
    l' k x = l' (to g) $ l'' x $ mapping g
      where
        g = fromJust $ M.lookup k (mappings a)
    l [] = []
    l (x : xs) = l' "seed" x : l xs

main :: IO ()
main = do
  content <- readFile "input/05.txt"
  -- content <- readFile "input/demo_05.txt"
  print $ getLowestLocation $ parseAlmanac content

-- print $ parseSeeds $ head (lines content)

-- print $ parseAlmanac content