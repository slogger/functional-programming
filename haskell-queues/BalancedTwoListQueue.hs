module BalancedTwoListQueue where
import Queue

data BalancedTwoListQueue a = BalancedTwoListQueue [a] [a] Int Int deriving Show

instance Queue BalancedTwoListQueue where
    create x = BalancedTwoListQueue [x] [] 1 0
    push x (BalancedTwoListQueue left right lengthLeft lengthRight)
        | (lengthRight + 1) > lengthLeft = BalancedTwoListQueue (left ++ reverse(right)) [x] (lengthLeft + lengthRight) 1
        | otherwise = BalancedTwoListQueue left (x:right)  lengthLeft (lengthRight+1)
    pop (BalancedTwoListQueue left right lengthLeft lengthRight)
        | null t = BalancedTwoListQueue (reverse right) [] lengthRight 0
        | (lengthLeft - 1) < lengthRight = BalancedTwoListQueue newLeft [] (lengthLeft+lengthRight-1) 0
        | otherwise = BalancedTwoListQueue t right (lengthLeft-1) lengthRight
        where
            t = tail left
            rev = reverse right
            newLeft = (t ++ rev)
    top (BalancedTwoListQueue left _ _ _) = head left
    empty (BalancedTwoListQueue left _ _ _) = null left
