import Data.Char (isDigit, isSpace)

parseInt :: String -> Maybe (Int, Int)
parseInt s =
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

parseInput :: [String] -> [(Int, Int)]
parseInput (time : distance : _) = zip (i' $ drop 5 time) (i' $ drop 9 distance)
  where
    i' s = case parseInt s of
      Nothing -> []
      Just (i, l) -> i : i' (drop l s)

calcBounds :: (Int, Int) -> (Int, Int)
calcBounds (t, d) = (l, h)
  where
    b = fromIntegral t / 2
    x = sqrt $ fromIntegral (t * t) / 4 - fromIntegral d
    l' = b - x
    h' = b + x
    l = if fromIntegral c - l' < 0.001 then c + 1 else c
      where
        c = ceiling l'
    h = if h' - fromIntegral c < 0.001 then c - 1 else c
      where
        c = floor h'

calcNum :: (Int, Int) -> Int
calcNum (l, h) = h - l + 1

main :: IO ()
main = do
  content <- readFile "input/06.txt"
  -- content <- readFile "input/demo_06.txt"
  print $ product $ map (calcNum . calcBounds) $ parseInput $ lines content
