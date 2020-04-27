-- Write a function, lastButOne , that returns the element before the last
lastButOne :: [a] -> a
lastButOne [] = error "Empty list!"
lastButOne [_] = error "List with one element!"
lastButOne [x,_] = x
lastButOne (x:xs) = lastButOne xs
