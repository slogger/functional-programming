module Queue where
-- abstract
class Queue q where
    push :: a -> q a -> q a
    pop :: q  a -> q a
    create :: a -> q a
    top :: q a -> a
    empty :: q a -> Bool
