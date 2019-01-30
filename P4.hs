--
-- CS 430 Spring 2019 P4 (Haskell 2)
--
-- Name: Nathan Chan
-- Date: 1/28/19
--

module P4 where
import Defs

-- calculate the result of an expression
eval :: Expr -> Int
eval e = case e of
    EInt e     -> e
    EAdd e1 e2 -> eval e1 + eval e2
    ESub e1 e2 -> eval e1 - eval e2
    EMul e1 e2 -> eval e1 * eval e2

-- count the total number of arithmetic operations in an expression
countOps :: Expr -> Int
countOps e = case e of
    EInt e     -> 0
    EAdd e1 e2 -> countOps e1 + countOps e2 + 1
    ESub e1 e2 -> countOps e1 + countOps e2 + 1
    EMul e1 e2 -> countOps e1 + countOps e2 + 1

-- calculate the height of the expression tree
-- height of one node = 1
height :: Expr -> Int
height e = case e of
    EInt e -> 1
    EAdd e1 e2 -> if (height e1 >  height e2) then height e1 + 1 else height e2 + 1
    ESub e1 e2 -> if (height e1 >  height e2) then height e1 + 1 else height e2 + 1
    EMul e1 e2 -> if (height e1 >  height e2) then height e1 + 1 else height e2 + 1

-- flatten the expression into a postfix string representation
-- use "(show i)" to convert int i to a string
postfix :: Expr -> String
postfix e = case e of
    EInt e -> show e
    EAdd e1 e2 -> postfix e1 ++ " " ++ postfix e2 ++ " +"
    ESub e1 e2 -> postfix e1 ++ " " ++ postfix e2 ++ " -"
    EMul e1 e2 -> postfix e1 ++ " " ++ postfix e2 ++ " *"

-- Finds all the unique ints in the tree.
uniq :: Eq t => [t] -> [t]
-- uniq (x:xs)
--     | elem x (uniq xs) = [y | y <- (uniq xs), y /= x]
--     | otherwise        = x : (uniq xs)
uniq xs = [x | (x, y) <- zip xs [0..], x `notElem` (take y xs)]

-- Sorts the unique ints.
sort :: Ord t => [t] -> [t]
sort [] = []
sort (p:xs) = (sort lesser) ++ [p] ++ (sort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

-- extract a sorted list of all unique integers in an expression
uniqInts :: Expr -> [Int]
uniqInts e = case e of
    EInt e ->  [e]
    EAdd e1 e2 -> sort(uniq(uniqInts e1 ++ uniqInts e2))
    ESub e1 e2 -> sort(uniq(uniqInts e1 ++ uniqInts e2))
    EMul e1 e2 -> sort(uniq(uniqInts e1 ++ uniqInts e2))


