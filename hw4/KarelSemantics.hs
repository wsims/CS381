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
                            then Error ("No beeper. Well too bad, so sad")
                            else OK (incBeeper p w) (decBag r)
stmt Move _ w r = let p = getPos r
                  in if isClear p w
                        then OK w (setPos p r)
                        else Error ("Blocked at : " ++ show p)
stmt (Turn d) _ w r = OK w (setFacing (cardTurn d (getFacing r)) r)

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
