{-# LANGUAGE BangPatterns #-}
import System.Environment
import System.IO
import Data.List
import Data.Tuple.Extra
import Data.Ord

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
          toObject i [w,v] = Object i (read w :: Weight) (read v :: Value)

main' bag_size !objects = do
    let objects' = sortOnRatio objects
        max_value' = realMaxVal objects' bag_size
        taken = bbMaxVal objects' bag_size
        indexes = map (index.snd) $ filter fst $ zip (snd taken) objects'
    putStrLn $ "realMaxVal = " ++ show max_value'
    putStrLn $ "bbMaxVal = " ++ show (fst taken, indexes)

sortOnRatio :: [Object] -> [Object]
sortOnRatio = sortOn (Data.Ord.Down . ratio)
    where ratio x = value x / weight x

greedyMaxVal :: (Object -> Weight -> Value) -> Value -> [Object] -> Weight -> Value
greedyMaxVal _ !v [] _ = v
greedyMaxVal othws !v (x:xs) bag_size
    | bag_size >= weight x = greedyMaxVal othws (v + value x) xs (bag_size - weight x)
    | otherwise = v + othws x bag_size

intMaxVal :: [Object] -> Weight -> Value
intMaxVal = greedyMaxVal (\_ _ -> 0) 0

realMaxVal :: [Object] -> Weight -> Value
realMaxVal = greedyMaxVal (\x bag_size -> (value x * bag_size) / weight x) 0

bbMaxVal :: [Object] -> Weight -> (Weight, [Bool])
bbMaxVal objects bag_size = (v, reverse b)
  where (v, b, _) = bbMaxVal' objects 0 bag_size [] 0

bbMaxVal' :: [Object] -> Value -> Weight -> [Bool] -> Value -> (Weight, [Bool], Value)
bbMaxVal' os value' bag_size bag max_val
  | bag_size < 0 || max_val > v = (0, [], 0)
  | [] <- os = (value', bag, max value' max_val)
  where v = value' + realMaxVal os bag_size
bbMaxVal' (o:os) value' bag_size bag max_val
  = if fst3 with >= fst3 without then with else without
  where with = bbMaxVal' os (value' + value o) (bag_size - weight o) (True:bag) max_val
        without = bbMaxVal' os value' bag_size (False:bag) $ thd3 with

