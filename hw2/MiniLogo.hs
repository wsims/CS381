module MiniLog where 

import Prelude hiding (Enum(..), sum)

-- Part 1: Our data types for MiniLogo 
type Numbers = Int
type Macro = String 
type Var = String 

data Mode = Up 
            | Down 
            deriving(Show)

data Expr = Input Var
            | NumberInput Numbers
            | Add Expr Expr
            deriving(Show, Eq)

type Prog = [Cmd]

data Cmd = Pen Mode 
           | Move (Expr, Expr)
           | Define Macro [Var] Prog
           | Call Macro [Expr]
           deriving(Show)

-- Part 2 
-- concrete syntax for the macro:
--  Define "line" (x1, x2, y1, y2) {
--       up, 
--       move (x1, y1)  
--       down
--       move (x2, y2)
-- }
--
line = Define "line" ["x1", "x2", "y1", "y2"] [Pen Up, Move (Input "x1", Input "y1"), Pen Down, Move (Input "x2", Input "y2"), Pen Up ]

-- Part 3 
-- concrete syntax for the macro: 
--  Define "nix" (x, y, w, h){
--       Call "line" (x, y, x+w, y+h)  
--       Call "line" (x, y+h, x+w, y)
--  }

nix = Define "nix" ["x", "y", "w", "h"] [ 
        Call "line" [Input "x", Input "y", Add (Input "x") (Input "w"), Add (Input "y") (Input "h")], 
        Call "line" [Input "x",  Add (Input "y") (Input "h"), Add (Input "x") (Input "w"), Input "y"]
      ]

-- Part 4
step :: Int -> Prog 
step 0 = []
step n = [
            Call "line" [NumberInput n , NumberInput n , NumberInput (n-1) , NumberInput n],
            Call "line" [NumberInput (n-1), NumberInput n, NumberInput (n-1), NumberInput (n-1)]       
         ] ++ step (n-1)

-- Part 5
--
-- | Returns a list of the names of all of the macros that are defined
--   
--   >>> macros [Define "line" ["x1"] [Pen Up]]
--   ["line"]
--
--   >>> macros [Define "line" ["x1"] [Define "nix" ["x1"] [Pen Up]]]
--   ["line","nix"]
--

macros :: Prog -> [Macro]
macros [] = []
macros (x:xs) = case x of
    Define m _ p -> (m:macros xs) ++ (macros_cmd (p!!0))
    otherwise -> macros xs

macros_cmd :: Cmd -> [Macro]
macros_cmd (Define m _ p) = [m] ++ (macros p) 
macros_cmd (Call _ _) = []
macros_cmd (Pen _) = []
macros_cmd (Move _) = []

-- Part 6







