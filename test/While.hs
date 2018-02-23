-- | A single register imperative language.
module While where


--
-- * Syntax
--

--
--  int  ::= (any integer)
--
--  expr ::= `R`                  -- load from register
--        |  int                  -- integer literal
--        |  expr `+` expr        -- addition expression
--
--  test ::= expr `≤` expr        -- less than or equal to
--
--  stmt ::= `R :=` expr          -- set register
--        |  `while` test stmt    -- while loop
--        |  `begin` stmt* `end`  -- statement block
--

data Expr = Get
          | Lit Int
          | Add Expr Expr
  deriving (Eq,Show)

data Test = LTE Expr Expr
  deriving (Eq,Show)

data Stmt = Set Expr
          | While Test Stmt
          | Begin [Stmt]
  deriving (Eq,Show)

-- Example program:
--   begin
--   R := 1
--   while R <= 100
--     R := R + R
--   end
p :: Stmt
p = Begin [
      Set (Lit 1), 
      While (LTE Get (Lit 100))
            (Set (Add Get Get))
    ]


--
-- * Semantics
--

-- | The current value of the register.
type Reg = Int

-- | Valuation function for expressions.
expr :: Expr -> Reg -> Int
expr Get       = \reg -> reg
expr (Lit i)   = \reg -> i
expr (Add l r) = \reg -> expr l reg + expr r reg

-- | Valuation function for tests.
test :: Test -> Reg -> Bool
test (LTE l r) = \reg -> expr l reg <= expr r reg

-- | Non-compositional valuation function for statements.
stmt :: Stmt -> Reg -> Reg
stmt (Set e)     = \reg -> expr e reg
stmt (While t s) = \reg -> if test t reg
                           then stmt (While t s) (stmt s reg)
                           else reg
stmt (Begin ss)  = \reg -> foldl (flip stmt) reg ss



-- | Compute least fix point. Defined in Data.Function.
fix f = let x = f x in x

-- | Compositional valuation function for statements using least fix point.
stmt' :: Stmt -> Reg -> Reg
stmt' (Set e)     = \reg -> expr e reg
stmt' (While t s) = fix (\f reg -> if test t reg
                                   then f (stmt s reg)
                                   else reg)
stmt' (Begin ss)  = \reg -> foldl (flip stmt') reg ss
