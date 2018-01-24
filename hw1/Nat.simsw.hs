-- Authors: Dan Lin (lintzu), Will Sims (simsw), Cameron Friel (frielc)
module Nat where

import Prelude hiding (Enum(..), sum)

-- NOTE: We did use this website https://gist.github.com/lucywyman/802f6c2b005b5526bcc6
-- as ref. Some of the functions were insipred from the functions found
-- on that website, as we used it to help us debug some issues we had. 

--
-- * Part 2: Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
         deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three

five :: Nat
five = Succ four


-- | The predecessor of a natural number.
--   
--   >>> pred Zero
--   Zero
--   
--   >>> pred three
--   Succ (Succ Zero)
--   
pred :: Nat -> Nat 
pred Zero = Zero 
pred (Succ num) = num 


-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool 
isZero a = a == pred(one)


-- | Convert a natural number to an integer.
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Int 
toInt Zero = 0 
toInt a = 1 + toInt (pred( a ))

-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--   
add :: Nat -> Nat -> Nat 
add a Zero = a 
add Zero a = a 
add (Succ a) (Succ b) = add(Succ(Succ(a))) b

-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--   
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat -> Nat -> Nat 
sub a Zero = a 
sub Zero a  = Zero
sub (Succ (a)) (Succ (b)) = sub a b       

-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
gt :: Nat -> Nat -> Bool 
gt Zero Zero  = False 
gt a Zero = True 
gt Zero b = False 
gt (Succ (a)) (Succ (b)) = gt a b 

-- | Multiply two natural numbers.
--
--   >>> mult two Zero
--   Zero
--
--   >>> mult Zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--
mult :: Nat -> Nat -> Nat
mult Zero a = Zero 
mult (Succ(a)) b = add (b) (mult a b )

-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--   
--   >>> sum [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
sum :: [Nat] -> Nat
sum [] = Zero 
sum (x:xs)  = foldr (add) x xs 

-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
odds :: [Nat]
odds = (Succ Zero) : map (Succ . Succ) odds 
