import Data.List
import Data.Maybe
import Data.Ord
import System.Environment
import System.IO

type Weight = Double
type Value = Double
data Object = Object {index :: Int, weight ::  Weight, value :: Value} deriving (Show)

-- | Le chemin du fichier du sac doit être passé en paramètre.
main :: IO ()
main = do
  fileName <- head <$> getArgs
  withFile fileName ReadMode process
  where toObject i [w,v] = Object i (read w :: Weight) (read v :: Value)
        process handle = do
          contents <- hGetContents handle
          let (bag_size:objects) = lines contents
              bag_size' = read bag_size :: Weight
              objects' = map (\(i, ws) -> toObject i $ words ws) $ zip [1..] objects
              taken = bbMaxVal objects' bag_size'
              indexes = sort $ map index $ snd taken
          putStrLn $ "Optimum value: " ++ show (fst taken)
          putStrLn $ "Selected items: " ++ show indexes

-- | La fonction 'bbMaxVal' prend une liste d'objets et la taille du sac
-- et renvoie la valeur maximale pouvant être contenue dans le sac ainsi
-- que les objets qui doivent y être mis pour l'atteindre. 
bbMaxVal :: [Object] -> Weight -> (Value, [Object])
bbMaxVal objects bag_size = fromJust $ bbMaxVal' (sortOnRatio objects) 0 bag_size [] 0

bbMaxVal' :: [Object] -> Value -> Weight -> [Object] -> Value -> Maybe (Value, [Object])
bbMaxVal' os value' bag_size bag max_val
  | bag_size < 0 || max_val > value' + greedyMaxVal os bag_size = Nothing
  | [] <- os = Just (value', bag)
bbMaxVal' (o:os) value' bag_size bag max_val
  = Just $ if fst with >= fst without then with else without
  where res = fromMaybe (value', bag)
        with = res $ bbMaxVal' os (value' + value o) (bag_size - weight o) (o:bag) max_val
        without = res $ bbMaxVal' os value' bag_size bag $ max max_val (fst with)

sortOnRatio :: [Object] -> [Object]
sortOnRatio = sortOn (Data.Ord.Down . ratio)
  where ratio x = value x / weight x

greedyMaxVal :: [Object] -> Weight -> Value
greedyMaxVal = f 0
  where
  f v [] _ = v
  f v (x:xs) bag_size
    | bag_size >= weight x = f (v + value x) xs (bag_size - weight x)
    | otherwise = v + (value x * bag_size) / weight x
