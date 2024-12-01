import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.List
import Data.Map
import GHC.Tuple qualified as Main

parseNumMap :: [String] -> (Map Int Int, Map (Int, Int) Int)
parseNumMap lines = (numMap, fieldMap)
  where
    parseInt' [] = []
    parseInt' (x : xs) = if isDigit x then x : parseInt' xs else []
    parseInt s = (read str :: Int, length str)
      where
        str = parseInt' s
    searchInts :: Int -> Int -> Int -> String -> [((Int, Int), [((Int, Int), Int)])]
    searchInts i py px s =
      case s of
        [] -> []
        (x : xs) ->
          if isDigit x
            then
              let (int, len) = parseInt s
               in ((i, int), unfoldr (\offx -> if offx == 0 then Nothing else Just (((py, px + offx - 1), i), offx - 1)) len) : searchInts (i + 1) py (px + len) (Data.List.drop len s)
            else searchInts i py (px + 1) xs
    parseNumMap' :: Int -> Int -> [String] -> [((Int, Int), [((Int, Int), Int)])]
    parseNumMap' _ _ [] = []
    parseNumMap' i py (x : xs) =
      nums ++ parseNumMap' (i + len) (py + 1) xs
      where
        nums = searchInts i py 0 x
        len = length nums
    (nums, fields) = Data.List.foldr (\lr n -> Data.Bifunctor.bimap (fst lr :) (snd lr :) n) ([], []) $ parseNumMap' 0 0 lines
    flatFields = concat fields
    numMap = Data.Map.fromList nums
    fieldMap = Data.Map.fromList flatFields

parseSymbols :: [String] -> [(Int, Int)]
parseSymbols = parseSymbols' 0
  where
    parseLineSymbols' _ _ [] = []
    parseLineSymbols' py px (x : xs) = if isSymbol x then (py, px) : parseLineSymbols' py (px + 1) xs else parseLineSymbols' py (px + 1) xs
      where
        isSymbol x = not (isDigit x || x == '.')
    parseSymbols' _ [] = []
    parseSymbols' py (x : xs) = parseLineSymbols' py 0 x ++ parseSymbols' (py + 1) xs

extrudeSymbols :: [(Int, Int)] -> [(Int, Int)]
extrudeSymbols [] = []
extrudeSymbols (x : xs) = a x ++ extrudeSymbols xs
  where
    difs = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    s (a, b) (c, d) = (a + c, b + d)
    a t = Data.List.map (s t) difs

intersectPositions :: Map (Int, Int) Int -> [(Int, Int)] -> [Int]
intersectPositions m p = nub $ sort $ Data.Map.foldr (:) [] $ Data.Map.intersection m pm
  where
    pm = fromList $ Data.List.map (,()) p

main :: IO ()
main = do
  content <- readFile "input/03.txt"
  -- content <- readFile "input/demo_03.txt"
  let lines = Data.List.lines content
  let (v, m) = parseNumMap lines
   in let res = Data.Map.foldr (+) 0 $ Data.Map.intersection v $ fromList . Data.List.map (,()) . intersectPositions m . s $ lines
       in print res
  where
    s = extrudeSymbols . parseSymbols