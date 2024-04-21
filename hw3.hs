module PE3 where

data Expression = Variable String
                | Constant Float
                | Sine Expression
                | Cosine Expression
                | Negation Expression
                | Addition Expression Expression
                | Multiplication Expression Expression
                
class Differential a where
    diff :: a -> a -> a


instance Show Expression where
  show (Variable name) = "Variable '" ++ name ++ "'"
  show (Constant value) = "Constant " ++ show value
  show (Sine expr) = "sin " ++ show expr
  show (Cosine expr) = "cos " ++ show expr
  show (Negation expr) = "-" ++ show expr
  show (Addition lhs rhs) = "(" ++ show lhs ++ " + " ++ show rhs ++ ")"
  show (Multiplication lhs rhs) = "(" ++ show lhs ++ " * " ++ show rhs ++ ")"



instance Eq Expression where
    (==) (Variable name1) (Variable name2) = name1 == name2
    (==) (Constant value1) (Constant value2) = value1 == value2
    (==) (Sine expr1) (Sine expr2) = expr1 == expr2
    (==) (Cosine expr1) (Cosine expr2) = expr1 == expr2
    (==) (Negation expr1) (Negation expr2) = expr1 == expr2
    (==) (Addition lhs1 rhs1) (Addition lhs2 rhs2) = lhs1 == lhs2 && rhs1 == rhs2
    (==) (Multiplication lhs1 rhs1) (Multiplication lhs2 rhs2) = lhs1 == lhs2 && rhs1 == rhs2
    (==) _ _ = False




instance Num Expression where
 fromInteger n = Constant (fromInteger n)
 (+) (Constant 0) expr = expr
 (+) expr (Constant 0) = expr
 (+) (Constant a) (Constant b) = Constant (a + b)
 (+) expr1 expr2 = Addition expr1 expr2
 (*) (Constant 0) _ = Constant 0
 (*) _ (Constant 0) = Constant 0
 (*) (Constant 1) expr = expr
 (*) expr (Constant 1) = expr
 (*) (Constant a) (Constant b) = Constant (a * b)
 (*) expr1 expr2 = Multiplication expr1 expr2
 negate (Constant a) = Constant (-a)
 negate expr = Negation expr
 abs (Constant x) = Constant (abs x)
 abs expr = expr
 signum (Constant x) = Constant (signum x)
 signum (Variable _) = Constant 1
 signum _ = Constant 1


instance Differential Expression where
 diff (Constant _) _ = Constant 0
 diff (Variable name) (Variable df_name) = if name == df_name then Constant 1 else Constant 0
 diff (Sine expr) (Variable dv) = Multiplication (Cosine expr) (diff expr (Variable dv))
 diff (Cosine expr) (Variable dv) = Multiplication (Negation (Sine expr)) (diff expr (Variable dv))
 diff (Negation expr) (Variable dv) = Negation (diff expr (Variable dv))
 diff (Addition lhs rhs) (Variable dv) = Addition (diff lhs (Variable dv)) (diff rhs (Variable dv))
 diff (Multiplication lhs rhs) (Variable dv) = Addition (Multiplication (diff lhs (Variable dv)) rhs) (Multiplication lhs (diff rhs (Variable dv)))






isOperator :: String -> Bool
isOperator is_op = if (elem is_op ["-","sin", "cos","*","+",")","("]) then True else False


pop_til_open :: [String] -> ([String], [String])
pop_til_open stack = span (/= "(") stack


pushOperators :: String -> [String] -> [String] -> ([String], [String])
pushOperators token [] outputStack = (outputStack, [token])
pushOperators token operatorStack outputStack
    | precedence token > precedence (head operatorStack) = (outputStack, token:operatorStack)
    | otherwise = pushOperators token (tail operatorStack) (outputStack ++ [head operatorStack])

shuntingyard :: [String] -> [String] -> [String] -> [String]
shuntingyard [] [] que = que
shuntingyard [] (operator:operators) que = shuntingyard [] operators (que ++ [operator])
shuntingyard (token:tokens) opSt que
    | not (isOperator token) = shuntingyard tokens opSt (que ++ [token])
    | token == "(" = shuntingyard tokens (token:opSt) que
    | token == ")" = let (poppedOperators, remainingOperators) = pop_til_open opSt
                         in shuntingyard tokens (tail remainingOperators) (que ++ poppedOperators)
    | otherwise = let (newQueue, newStack) = pushOperators token opSt que
                      in shuntingyard tokens newStack newQueue
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       
precedence :: String -> Int
precedence op
            |op == "-" = 5
            |op == "sin" = 4
            |op == "cos" = 4
            |op == "*" = 3
            |op == "+" = 2
            |op == ")" = 0
            |op == "(" = 0
                                                                       
                                                                       
                                                                       

