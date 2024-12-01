import Data.Char (isDigit)
import Data.String (lines)

data Cubes = Cubes {red, green, blue :: Int} deriving (Show)

(<!) :: Cubes -> Cubes -> Bool
(<!) (Cubes r1 g1 b1) (Cubes r2 g2 b2) = r2 <= r1 && g2 <= g1 && b2 <= b1

(+>) :: Cubes -> Cubes -> Cubes
(+>) (Cubes r1 g1 b1) (Cubes r2 g2 b2) = Cubes (r1 + r2) (g1 + g2) (b1 + b2)

data Game = Game {getId :: Int, cubes :: [Cubes]} deriving (Show)

skip :: Int -> String -> String
skip 0 xxs = xxs
skip c (_ : xs) = skip (c - 1) xs

int :: String -> (Int, String)
int =
  int' []
  where
    int' c [] = (read c :: Int, [])
    int' c (x : xs) = if isDigit x then int' (x : c) xs else (read $ reverse c :: Int, x : xs)

split :: Char -> String -> [String]
split sep s =
  let (x, xs) = sp' [] s
   in if null xs then [x] else x : split sep xs
  where
    sp' t [] = (reverse t, [])
    sp' t (x : xs) = if x /= sep then sp' (x : t) xs else (reverse t, xs)

parsePartialCubes :: String -> Cubes
parsePartialCubes s =
  let (num, pc) = int s
   in case skip 1 pc of
        "red" -> Cubes num 0 0
        "green" -> Cubes 0 num 0
        "blue" -> Cubes 0 0 num

parseCubes :: String -> Cubes
parseCubes s = foldl1 (+>) $ map (parsePartialCubes . skip 1) (split ',' s)

deserializeGame :: String -> Game
deserializeGame line = Game id hands
  where
    s1 = skip 5 line
    (id, s2) = int s1
    hands = map parseCubes $ split ';' $ skip 1 s2

filterGame :: Cubes -> Game -> Bool
filterGame maxCubes game = all (maxCubes <!) $ cubes game

main :: IO ()
main = do
  -- content <- readFile "input/demo_02.txt"
  content <- readFile "input/02.txt"
  print $ sum $ ids content
  where
    games = map deserializeGame . lines
    filtered = filter (filterGame (Cubes 12 13 14)) . games
    ids = map getId . filtered
