-- Authors: Dan Lin (lintzu), Will Sims (simsw), Cameron Friel (frielc)
module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState
import KarelExamples --imported so that first two test cases pass


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) world robot = not (test t world robot)
test (Facing dir) _ robot = dir == getFacing robot
test (Beeper) world robot = hasBeeper (getPos robot) world
test (Empty) _ robot = isEmpty robot
test (Clear direction) world robot = isClear (relativePos direction robot) world

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                            then OK (decBeeper p w) (incBag r)
                            else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper _ w r = let p = getPos r
                       in if isEmpty r
                            then Error ("No beeper to put.")
                            else OK (incBeeper p w) (decBag r)
stmt Move _ w (p, c, b) = let np = neighbor c p
                          in if isClear np w
                                then OK w (setPos np (p, c, b))
                                else Error ("Blocked at: " ++ show np)
stmt (Turn d) _ w r = OK w (setFacing (cardTurn d (getFacing r)) r)
stmt (Block []) _ w r = OK w r
stmt (Block (x:xs)) d w r = case stmt x d w r of
                            OK w' r' -> stmt (Block xs) d w' r'
                            Done r' -> Done r'
                            Error m -> Error m
stmt (If t s1 s2) d w r = case test t w r of
                        t' -> if t' then stmt s1 d w r else stmt s2 d w r
stmt (Call m) d w r = case lookup m d of
                            (Just b) -> stmt b d w r
                            _        -> Error ("Undefined macro: " ++ m)
stmt (Iterate i b)  d w r = if i > 0 then case stmt b d w r of
                                OK w' r' -> stmt (Iterate (i-1) b) d w' r'
                                Done r' -> Done r'
                                Error m -> Error m
                            else OK w r
stmt (While t b) d w r = if test t w r then case stmt b d w r of
                                OK w' r' -> stmt (While t b) d w' r'
                                Done r' -> Done r'
                                Error m -> Error m
                            else OK w r

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
