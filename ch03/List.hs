-- Write the converse of fromList for the List type: a function that takes a List a and generates a [a]

data List a = Cons a (List a) | Nil deriving (Show)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x Nil) = [x]
fromList (Cons x xs) = x:(fromList xs)
