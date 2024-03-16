
data Tree alpha = Node (alpha, Tree alpha, Tree alpha) | Empty deriving Show



get :: Eq alpha=> Tree alpha -> [alpha] -> Maybe (Tree alpha)

get Empty _ = Nothing
get (Node (value ,left, right)) [x] = if value == x then Just (Node (value ,left, right)) else Nothing
--get tree [x] = Just tree --böyle de yzabilirdik yani yukarda tanımda tipleri belirttiğimiz için np
-- vay amına koyayım türü belirtsek sıkıntı çıkmıyo ama biz değer kullanmak istediğimiz için genellikle belirtiyoruz
get (Node (value, left, right)) (x:xs) =
  if value == x
    then case get left xs of
           Just subtree -> Just subtree
           Nothing -> get right xs
    else Nothing


set :: Eq alpha => Tree alpha -> [alpha] -> Tree alpha -> Tree alpha
set Empty _ _ = Empty
set (Node (value,  left, right)) [alpha] (Node(value2, left2, right2)) = if value == alpha then (Node(value2, left2, right2)) else (Node(value, left, right))
set (Node (value,  left, right)) (x:xs) (Node(value2, left2, right2)) = if value == x
                                                                        then case get left xs of 
                                                                         Just tree -> (Node (value,  set left xs (Node(value2, left2, right2)), right)) 
                                                                         Nothing -> case get  right xs of 
                                                                            Just tree ->  (Node (value,  left, set right xs (Node(value2, left2, right2)))) 
                                                                            Nothing -> (Node (value,  left, right)) else (Node (value,  left, right))



dene x [first] = x
dene x (first:rem) = first + x

t4 = Node (9,Node (1,Node (2,Node (3,Node (4,Empty,Empty),Empty),
 Node (1,Empty,Empty)),Node (7,Empty,Empty)),
 Node (2,Node (3,Node (4,Empty,Empty),Empty), Node (1,Empty,Empty)))
main::IO()
main = do

    let z = get t4 [1]
    let x = set t4 [9] (Node (11,Node (10,Empty,Empty),Empty))
    putStrLn $ "işte bu " ++ show z ++ "ve bu da denediğim fonksiyon" ++ show (dene 1 [1])