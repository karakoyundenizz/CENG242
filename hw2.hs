module PE2 where
import GHC.Exts.Heap.Closures (GenClosure(key))

data Tree k v = EmptyTree | Node k v [Tree k v] deriving (Show, Eq)

-- Question 1
selectiveMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
selectiveMap _ _ [] = []
selectiveMap func1 func2 (x:xs) | func1 x = (func2 x):selectiveMap func1 func2 xs
                                | otherwise = x: selectiveMap func1 func2 xs






-- Question 2
tSelectiveMap :: (k -> Bool) -> (v -> v) -> Tree k v -> Tree k v
tSelectiveMap _ _ EmptyTree = EmptyTree
tSelectiveMap func1 func2 (Node key value list ) | func1 key = Node key (func2 value) (map (tSelectiveMap func1 func2) list)
                                | otherwise =  Node key value (map (tSelectiveMap func1 func2) list)





-- Question 3
{-tSelectiveMappingFold :: (k -> Bool) -> (k -> v -> r) -> (r -> r -> r) -> r -> Tree k v -> r
tSelectiveMappingFold _ _ _ idt _ = idt-}



tSelectiveMappingFold :: (k -> Bool) -> (k -> v -> r) -> (r -> r -> r) ->
                         r -> Tree k v -> r
tSelectiveMappingFold p f comb acc (Node k v []) = if p k then comb (f k v ) acc else acc
tSelectiveMappingFold p f comb acc (Node k v children) =
    if p k then comb (f k v) (foldr (\child acc -> tSelectiveMappingFold p f comb acc child) acc children)
    else (foldr (\child acc -> tSelectiveMappingFold p f comb acc child) acc children)



-- Question 4
-- This question commented out so that your file compiles even before you complete it
-- It shouldn't effect the grades of other questions when commented out
-- When ready, just remove the lines with {- and -} to uncomment the function





searchTree :: (Eq v, Eq k) => v -> Tree k v -> (k -> v)
searchTree def = tSelectiveMappingFold a b c d
    where  {-a = (\x  -> True)
           b = (\x y -> x)
           c = (\x y z -> if x == z then )
           d =  (\x-> if x == )-}
             a = (\_ -> True)  
             b = (\k1 v k2 -> if k1 == k2 then v else def)  
             c = (\f1 f2 -> \k -> if f1 k /= def then f1 k else f2 k)
             d = \_ -> def 




main = do
        let x = selectiveMap even (\x -> x*x) [1,2,3,4,5,6,7]
            t = Node 2 3 [Node (-2) (-4) [], Node (-1) 7 [], Node 2 (-3) []]
            u = Node (-2) 3 [Node (-2) (-4) [], Node (-1) 7 [], Node 2 (-3) []]
            searchT = searchTree 0 u

        putStrLn $ "Bu selectiveMap: " ++ show x ++ " Bu da tselectiveMap: " ++ show (tSelectiveMap (<0) (\x -> x*x) t) ++ " bu da tfold: "++ show (searchT (2))