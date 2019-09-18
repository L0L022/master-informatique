import System.IO
import Data.List

type Weight = Integer
type Value = Integer
data Object = Object {weight ::  Weight, value :: Value} deriving (Show)

main = do
    withFile "sac0" ReadMode process
    where process handle = do
            contents <- hGetContents handle
            let (bag_size:objects) = lines contents
                bag_size' = read bag_size :: Weight
                objects' = map (toObject.words) objects
            main' bag_size' objects'
          toObject (w:v:[]) = Object (read w :: Weight) (read v :: Value)

main' bag_size objects = do
    putStrLn $ "bag size = " ++ show bag_size ++ "\n" ++ show objects
    let objects' = sort_on_ratio objects
        max_value = relax_frac objects' bag_size
    putStrLn $ "max value = " ++ show max_value

sort_on_ratio :: [Object] -> [Object]
sort_on_ratio = sortOn ratio
    where ratio x = v x / w x
          v = toRational.value
          w = toRational.weight

relax_frac :: [Object] -> Weight -> Value
relax_frac [] _ = 0
relax_frac _ bag_size
    | bag_size <= 0 = 0
relax_frac (x:xs) bag_size = value x + relax_frac xs (bag_size - weight x)
