module BalancedTwoListQueue where
import Queue

-- TODO: Понять что написать в качестве 3 и 4 аргумента
data BalancedTwoListQueue a = BalancedTwoListQueue [a] [a] a a deriving Show

instance Queue BalancedTwoListQueue where
    create x = BalancedTwoListQueue [x] [] (length x) 0
    push x (BalancedTwoListQueue left right lengthLeft lengthRight)
        | (lengthRight + 1) > lengthLeft = BalancedTwoListQueue (left:reverse(right)) [x] (lengthLeft + lengthRight) 1
        | otherwise = BalancedTwoListQueue left (x:right)  lengthLeft lengthRight+1
    pop (BalancedTwoListQueue left right lengthLeft lengthRight)
        | null t = BalancedTwoListQueue (reverse right) [] lengthRight 0
        | (lengthLeft - 1) < lengthRight = (t:reverse(right)) [] (lengthLeft+lengthRight-1) 0
        | otherwise = BalancedTwoListQueue t right lengthLeft-1 lengthRight
        where
            t = tail left
    top (BalancedTwoListQueue left _ _ _) = head left
    empty (BalancedTwoListQueue left _ _ _) = null left
