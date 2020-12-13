module Sorts.InsertionSort where

listToSort = [13, 2, 3, 14, 17, 4, 1, 5, 16, 12, 9, 10, 15, 8, 7, 11, 18, 19, 6, 20]

-- 挿入ソート : 整列済みのリストに対し、適切な場所に値を挿入する

insertionSort:: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert x (insertionSort xs)

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
-- @ : アズパターン、_の反対で、照合に常に成功する(?)
insert x lst@(y:ys) = if x <= y then x:lst else tracey:(insert x ys)

main = do
  putStrLn $ "Unsorted: " ++ show listToSort
  putStrLn $ "Sorted: " ++ show (insertionSort listToSort)

