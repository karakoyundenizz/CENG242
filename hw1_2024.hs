{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module PE1 where

-- Question 1
findInList :: (Eq a) => [a] -> [a] -> [Int]
findInList _ [] = []
findInList [] _ = []
findInList list1 list2 = go 0 list1 list2
  where
    go _ [] _ = []
    go _ _ [] = []
    go idx (x:xs) (y:ys)
      | x == y = idx : go (idx + 1) xs ys
      | otherwise = go (idx + 1) (x:xs) ys


-- Question 2
findInMatrix :: (Eq a) => [[a]] -> [[a]] -> [[(Int, Int)]]
findInMatrix _ [] = []
findInMatrix [] _ = []
findInMatrix pattern matrix =
                                let listOfAllCoordinates = set_matrix (length matrix-1) (length (head matrix)-1)
                                    lengths_of_patterns = map(\x -> length x) pattern
                                    listOfAllNumbers_inMatrix = get_matrix  matrix listOfAllCoordinates 
                                    listOfAllPatterns'Coordinate = findInList (concat_pattern pattern) listOfAllNumbers_inMatrix
                                    result_coordinat = map(\x -> listOfAllCoordinates !! x) listOfAllPatterns'Coordinate
                                    splitted_coordinate_result = splitByCounts lengths_of_patterns result_coordinat
                                        in splitted_coordinate_result 

set_matrix :: Int -> Int -> [(Int, Int)]
set_matrix row col = concatMap (\sum -> [(x, sum - x) | x <- [max 0 (sum - col)..min sum row]]) [0..row+col]

concat_pattern :: [[a]] -> [a]
concat_pattern pattern = foldr (++) [] pattern

splitByCounts :: [Int] -> [a] -> [[a]]
splitByCounts [] _ = []
splitByCounts _ [] = []
splitByCounts (n:ns) xs = chunk : splitByCounts ns rest
  where
    (chunk, rest) = splitAt n xs




get_matrix :: [[a]] -> [(Int, Int)] -> [a]
get_matrix grid matrix = [(grid !! a) !! b | (a,b) <- matrix]



--final = map ( \x-> [kor_list !! a|a<-(take x whole) ] )  lengths_of_pattern
--pattern_cooordinates = map ( \x-> [kor_list !! a|a<-findInList x list ] )  pattern
--in pattern_cooordinates


main::IO()
main = do
       let b = foldl (++) [] [[1,2,3,4],[1,2,3,4],[5,6,7,8]]
           a =  foldr (++) [] [[1,2,3,4],[1,2,3,4],[5,6,7,8]]
           x = set_matrix  2 3
           s = splitAt 1 [1,2,3,4]
           z = get_matrix  [[1,2,3,4],[1,2,3,4],[5,6,7,8]] x
           y = findInMatrix [[1,5],[3,4]] [[1,2,3,4],[1,2,3,4],[5,6,7,8]]
       putStrLn $ " bu x  " ++show x ++ "bbu b  "++ show b ++ "bbu s  " ++ show s