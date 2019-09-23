{-# LANGUAGE BangPatterns #-}
import System.Environment
import System.IO
import Data.List
import Data.Tuple.Extra

type Weight = Double
type Value = Double
data Object = Object {index :: Int, weight ::  Weight, value :: Value} deriving (Show)

main = do
    fileName <- head <$> getArgs
    withFile fileName ReadMode process
    where process handle = do
            contents <- hGetContents handle
            let (bag_size:objects) = lines contents
                bag_size' = read bag_size :: Weight
                objects' = map (\(i, ws) -> toObject i $ words ws) $ zip [1..] objects
            main' bag_size' objects'
          toObject i (w:v:[]) = Object i (read w :: Weight) (read v :: Value)

main' bag_size !objects = do
    let objects' = sort_on_ratio objects
        max_value' = real_max_val objects' bag_size
        taken = bb_max_val objects' bag_size
        indexes = map (index.snd) $ filter fst $ zip (snd taken) objects'
    putStrLn $ "real_max_val = " ++ show max_value'
    putStrLn $ "bb_max_val = " ++ show indexes

sort_on_ratio :: [Object] -> [Object]
sort_on_ratio = reverse.sortOn ratio
    where ratio x = value x / weight x

greedy_max_val :: (Object -> Weight -> Value) -> Value -> [Object] -> Weight -> Value
greedy_max_val _ v [] _ = v
greedy_max_val othws v (x:xs) bag_size
    | bag_size >= weight x = greedy_max_val othws (v + value x) xs (bag_size - weight x)
    | otherwise = othws x bag_size

int_max_val :: [Object] -> Weight -> Value
int_max_val = greedy_max_val (\_ _ -> 0) 0

real_max_val :: [Object] -> Weight -> Value
real_max_val = greedy_max_val (\x bag_size -> (value x * bag_size) / weight x) 0

bb_max_val :: [Object] -> Weight -> (Weight, [Bool])
bb_max_val objects bag_size = (v, reverse b)
  where (v, b, _) = bb_max_val' objects 0 bag_size [] 0

bb_max_val' :: [Object] -> Value -> Weight -> [Bool] -> Value -> (Weight, [Bool], Value)
bb_max_val' os value' bag_size bag max_val
  | bag_size < 0 || max_val > v = (0, [], 0)
  | [] <- os = (value', bag, max value' max_val)
  where v = value' + real_max_val os bag_size
bb_max_val' (o:os) value' bag_size bag max_val
  = if fst3 with >= fst3 without then with else without
  where with = bb_max_val' os (value' + value o) (bag_size - weight o) (True:bag) max_val
        without = bb_max_val' os value' bag_size (False:bag) $ thd3 with
