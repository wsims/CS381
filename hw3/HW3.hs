module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen Down) (m, (x, y)) = ((Down, (x, y)), Nothing);
cmd (Pen Up) (m, (x, y)) = ((Up, (x, y)), Nothing);
cmd (Move x y) (Down, p) = ((Down, (x, y)), Just (p, (x, y)))
cmd (Move x y) (Up, p) = ((Up, (x, y)), Nothing)

-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] = \s -> (s, [])
prog (c:cs) = \s -> case cmd c s of
                        ((Down, (x, y)), Nothing) -> prog cs (Down, (x, y))
                        ((Up, (x, y)), Nothing) -> prog cs (Up, (x, y))
                        ((Down, (x, y)), Just (p, (a, z))) -> (\(s, cs) -> (s, (p, (a, z)):cs)) (prog cs (Down, (x, y)))

--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = (spiral 45 3 ++ body 45 3 ++ leftEye 45 3 ++ rightEye 45 3 ++ spiral 20 3 ++ body 20 3 ++ leftEye 20 3 ++ rightEye 20 3 ++ spiral 70 3 ++ body 70 3 ++ leftEye 70 3 ++ rightEye 70 3) 

spiral :: Int -> Int -> Prog
spiral x y = [Pen Up, Move (x) (y), Pen Down, Move (x) (y+10), Move (x-10) (y+10),
  Move (x-10) (y), Move (x-2) (y), Move (x-2) (y+8), Move (x-8) (y+8), Move (x-8) (y+2), Move (x-4) (y+2), Move (x-4) (y+6), Move (x-6) (y+6), Move (x-6) (y+4)]

body :: Int -> Int -> Prog
body x y = [Pen Up, Move (x) (y), Pen Down, Move (x+1) (y), Move (x+3) (y+2), Move (x+5) (y+2),Move (x+5) (y), Move (x+2) (y-3), Move (x-14) (y-3), Move (x-11) (y), Move (x) (y)]

leftEye :: Int -> Int -> Prog
leftEye x y = [Pen Up, Move (x+3) (y+2), Pen Down, Move (x+1) (y+6)]

rightEye :: Int -> Int -> Prog
rightEye x y  = [Pen Up, Move (x+5) (y+2), Pen Down, Move (x+7) (y+6)]
