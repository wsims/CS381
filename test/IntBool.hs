-- | A simple expression language with two types.
module IntBool where

import Prelude hiding (not,and,or)


--  Syntax of the "core" IntBool language:
--
--  int  ::=  (any integer)
--  bool ::=  true  |  false
--
--  exp  ::=  int                integer literal
--        |   exp + exp          integer addition
--        |   exp * exp          integer multiplication
--        |   exp = exp          check whether two values are equal
--        |   exp ? exp : exp    conditional expressions


-- 1. Define the abstract syntax as a Haskell data type.

data Exp = Lit Int
         | Add Exp Exp
         | Mul Exp Exp
         | Equ Exp Exp
         | If  Exp Exp Exp
  deriving (Eq,Show)


-- Here are some example expressions:
--  * draw the abstract syntax trees (exercise)
--  * what should the result be?
ex1 = Mul (Lit 2) (Add (Lit 3) (Lit 4))  -- 2*(3+4)            =>  14
ex2 = Equ ex1 (Lit 10)                   -- (2*(3+4)) = 10     =>  false
ex3 = If ex1 (Lit 5) (Lit 6)             -- (2*(3+4)) ? 5 : 6  =>  type error!


-- 2. Identify/define the semantic domain for this language
--   * what types of values can we have?
--   * how can we express this in Haskell?

data Value = I Int
           | B Bool
           | TypeError
  deriving (Eq,Show)


-- data Maybe a    = Just a | Nothing
-- data Either a b = Left a | Right b

-- Alternative semantics domain using Maybe and Either:
--
--   type Value = Maybe (Either Int Bool)
--
-- Example semantic values in both representations:
--
--   I 14       <=>  Just (Left 14)
--   B False    <=>  Just (Right False)
--   TypeError  <=>  Nothing


-- 3. Define the semantic function
sem :: Exp -> Value
sem (Lit i)    = I i
sem (Add l r)  = case (sem l, sem r) of
                   (I i, I j) -> I (i+j)
                   _ -> TypeError
sem (Mul l r)  = case (sem l, sem r) of
                   (I i, I j) -> I (i*j)
                   _ -> TypeError
sem (Equ l r)  = case (sem l, sem r) of
                   (I i, I j) -> B (i == j)
                   (B b, B c) -> B (b == c)
                   _ -> TypeError
sem (If c t e) = case sem c of
                   B b -> if b then sem t else sem e
                   _ -> TypeError



-- 4. Syntactic sugar.
--
-- Goal: extend the syntax of our language with the following operations:
--
--      * boolean literals
--      * integer negation
--      * boolean negation (not)
--      * conjunction (and)
--      * disjunction (or)
-- 
-- How do we do this? Can we do it without changing the semantics?

true :: Exp
true = Equ (Lit 0) (Lit 0)

false :: Exp
false = Equ (Lit 0) (Lit 1)

neg :: Exp -> Exp
neg e = Mul (Lit (-1)) e

not :: Exp -> Exp
not e = If e false true

and :: Exp -> Exp -> Exp
and l r = If l r false

or :: Exp -> Exp -> Exp
or l r = If l true r

-- Example program that uses our syntactic sugar.
ex4 :: Exp
ex4 = or (not true)
         (and (Equ (neg (Lit 3)) (Lit (-3)))
              (or true false))


--
-- * Statically typed variant (now!)
--

-- 1. Define the syntax of types
data Type = TInt | TBool | TError
  deriving (Eq,Show)

-- 2. Define the typing relation.
typeOf :: Exp -> Type
typeOf (Lit i)    = TInt
typeOf (Add l r)  = case (typeOf l, typeOf r) of
                      (TInt, TInt) -> TInt
                      _ -> TError
typeOf (Mul l r)  = case (typeOf l, typeOf r) of
                      (TInt, TInt) -> TInt
                      _ -> TError
typeOf (Equ l r)  = case (typeOf l, typeOf r) of
                      (TInt, TInt) -> TBool
                      (TBool, TBool) -> TBool
                      _ -> TError
typeOf (If c t e) = case (typeOf c, typeOf t, typeOf e) of
                      (TBool, TInt, TInt) -> TInt
                      (TBool, TBool, TBool) -> TBool
                      _ -> TError

-- typeOf (If c t e) = case (typeOf c, typeOf t, typeOf e) of
--                      (TBool, tt, et) | tt == et -> tt
--                      _ -> TError


-- 3. Define the semantics of type-correct programs.
sem' :: Exp -> Either Int Bool
sem' (Lit i)    = Left i
sem' (Add l r)  = Left (evalInt l + evalInt r)
sem' (Mul l r)  = Left (evalInt l * evalInt r)
sem' (Equ l r)  = Right (sem' l == sem' r)
sem' (If c t e) = if evalBool c then sem' t else sem' e

-- | Helper function to evaluate an Exp to an Int.
evalInt :: Exp -> Int
evalInt e = case sem' e of
              Left i -> i
              _ -> error "internal error: expected Int got something else!"

-- | Helper function to evaluate an Exp to an Bool.
evalBool :: Exp -> Bool
evalBool e = case sem' e of
               Right b -> b
               _ -> error "internal error: expected Bool got something else!"

-- 4. Define our interpreter.
eval :: Exp -> Value
eval e = case typeOf e of
           TInt   -> I (evalInt e)
           TBool  -> B (evalBool e)
           TError -> TypeError
