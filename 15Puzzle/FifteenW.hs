{--------------------------------------------
  15 Puzzle Solver
  Copyright (C) 2018 Hideyuki Kawabata

  -- specialized version

*Main> main
Input a scrambled pattern:
[[5,13,3,7],[1,14,0,15],[6,4,10,8],[9,2,11,12]] 
Initial state:
[ 9][ 2][11][12]
[ 6][ 4][10][ 8]
[ 1][14][  ][15]
[ 5][13][ 3][ 7]
Solution: 
--[1,2]----------------------------
 D R R U L D D R U U L L D D R U
--[5,6]----------------------------
 U R U L D D R U L U L D D R U
--[3,4]----------------------------
 L U L D D R U R U L D R U L L D D D R U U L D D R U
--[9,13]---------------------------
 U R R U L D R U L L D L D R U L U R R R D L L U R R D L
--[7,8]----------------------------
 L L U R D L U R R D L L U R D D L U R D L U R R U L L D D R U
--[10,14]--------------------------
 L U R D L U R R D L U R D L
--[11,12,15]-----------------------
 L U
Final state:
[ 1][ 2][ 3][ 4]
[ 5][ 6][ 7][ 8]
[ 9][10][11][12]
[13][14][15][  ]

 --------------------------------------------}

import Control.Monad
import Control.Monad.Writer

type Point = (Int, Int)

data OP = D | U | L | R | 
          S12 | S56 | S34 | S913 | S78 | S1014 | S1115
        deriving (Eq, Ord, Show, Read)

oneStep :: [Point] -> [Point]
oneStep [] = []
oneStep [(xs, ys)] = do
  (x, y) <- [(xs, ys+1), (xs, ys-1), (xs+1, ys), (xs-1, ys)]
  guard $ x >= 0 && x <= 3 && y >= 0 && y <= 3
  return (x, y)

nSteps :: Int -> [Point] -> [Point] -> [[Point]]
nSteps 0 l tl = return l
nSteps n l tl = do
  h' <- oneStep $ take 1 l
  guard $ not $ elem h' tl
  guard $ not $ elem h' l
  nSteps (n-1) (h':l) tl

findPaths :: Point -> Point -> [Point] -> [[Point]]
findPaths s e tl = do
  n <- [0..]
  l <- nSteps n [s] tl
  guard $ take 1 l == [e]
  return l

findOnePath :: Point -> Point -> [Point] -> [Point]
findOnePath s e tl = head $ findPaths s e tl

----------------------------------------------------


type Board = [[Int]]
t :: Board
t = [[1,7,6,0],[14,9,2,12],[8,15,5,13],[3,4,11,10]]

findPosX m [] k = k -- dummy
findPosX m (h:t) k
  | m == h = k
  | otherwise = findPosX m t (k+1)

findPos :: Int -> Board -> Point
findPos m l = findPos' m l 0 0
findPos' m [] x y = (x, y) -- dummy
findPos' m (h:t) x y
  | elem m h = (findPosX m h x, y)
  | otherwise = findPos' m t x (y+1)

pathToOp :: [Point] -> [OP]
pathToOp [] = [] -- dummy
pathToOp [p] = []
pathToOp ((x2,y2):(x1,y1):t)
  | x2 == x1 = (if y2 > y1 then D else U) : pathToOp ((x1,y1):t)
  | otherwise = (if x2 > x1 then L else R) : pathToOp ((x1,y1):t)


applyOp :: [OP] -> Board -> Board
applyOp [] f = f
applyOp (x:xs) f = applyOp xs $ move x f

move :: OP -> Board -> Board
move op f
  | op == R || op == L = [moveRL op $ f!!0, 
                          moveRL op $ f!!1,
                          moveRL op $ f!!2,
                          moveRL op $ f!!3]
  | op == U || op == D = [moveUD op [-1,-1,-1,-1] (f!!0) (f!!1),
                          moveUD op (f!!0) (f!!1) (f!!2),
                          moveUD op (f!!1) (f!!2) (f!!3),
                          moveUD op (f!!2) (f!!3) [-1,-1,-1,-1]]
  | otherwise = f

moveRL R [x, y, z, 0] = [x, y, 0, z]
moveRL R [x, y, 0, z] = [x, 0, y, z]
moveRL R [x, 0, y, z] = [0, x, y, z]
moveRL R q = q
moveRL L [0, x, y, z] = [x, 0, y, z]
moveRL L [x, 0, y, z] = [x, y, 0, z]
moveRL L [x, y, 0, z] = [x, y, z, 0]
moveRL L q = q

moveUD D [a, b, c, d] [e, f, g, h] [i, j, k, l] 
  | a == 0 = [0, f, g, h]
  | b == 0 = [e, 0, g, h]
  | c == 0 = [e, f, 0, h]
  | d == 0 = [e, f, g, 0]
  | e == 0 && i /= -1 = [i, f, g, h]
  | f == 0 && j /= -1 = [e, j, g, h]
  | g == 0 && k /= -1 = [e, f, k, h]
  | h == 0 && l /= -1 = [e, f, g, l]
  | otherwise = [e, f, g, h]
moveUD U [a, b, c, d] [e, f, g, h] [i, j, k, l] 
  | e == 0 && a /= -1 = [a, f, g, h]
  | f == 0 && b /= -1 = [e, b, g, h]
  | g == 0 && c /= -1 = [e, f, c, h]
  | h == 0 && d /= -1 = [e, f, g, d]
  | i == 0 = [0, f, g, h]
  | j == 0 = [e, 0, g, h]
  | k == 0 = [e, f, 0, h]
  | l == 0 = [e, f, g, 0]
  | otherwise = [e, f, g, h]


----------------------------------------------------

-- Move a tile (m) to a point (gl) with respect to a taboo list (tl)
moveTile :: Int -> Point -> [Point] -> Board -> Writer [OP] Board
moveTile m gl tl bd = do
  let s = findPos m bd
  if s == gl then return bd
             else moveTileOneStep s gl tl bd >>= moveTile m gl tl

-- Move a tile at a point (s) by one step bound for a point (gl)
moveTileOneStep :: Point -> Point -> [Point] -> Board -> Writer [OP] Board
moveTileOneStep s gl tl bd = do
  let t = head $ drop 1 $ reverse $ findOnePath s gl tl -- 0's target
  move0 t (s:tl) bd -- move blank
   >>= applyOpW (reverse $ pathToOp [s, t]) -- additional move

-- Move blank to a point (gl)
move0 :: Point -> [Point] -> Board -> Writer [OP] Board
move0 gl tl bd = applyOpW ops bd
  where z = findPos 0 bd
        ops = reverse $ pathToOp $ findOnePath z gl tl
  
-- Move a tile along the designated path
applyOpW :: [OP] -> Board -> Writer [OP] Board
applyOpW ops bd = writer (applyOp ops bd, ops)


move1 :: Board -> Writer [OP] Board
move1 bd = do
  tell [S12]
  bd12 <- moveTile 1 (0,3) [] bd >>= moveTile 2 (1,3) [(0,3)]
  tell [S56]
  moveTile 5 (0,2) [(0,3),(1,3)] bd12
    >>= moveTile 6 (1,2) [(0,3),(1,3),(0,2)]
  

{-
  1 2 A B
  5 6 C D   target: C=4,E=3 or F=4,D=3 --> A=3,B=4
  - - E F
  - - - -
-}
move34 :: Board -> Writer [OP] Board
move34 bd = move34x bd [(2,3),(3,3),(2,2),(3,2),(2,1),(3,1)] S34 3 4
move34x bd [pa, pb, pc, pd, pe, pf] op m m' = do
  let (lft, rgt, up, dwn) = (head $ pathToOp [pb, pa],
                             head $ pathToOp [pa, pb],
                             head $ pathToOp [pc, pa],
                             head $ pathToOp [pa, pc])
  let tl = [(0,2),(1,2),(1,3)]
  tell [op]
  bd' <- move34' bd [pa, pb, pc, pd, pe, pf] m m'
  let pos3 = findPos m bd'
  if pos3 == pe then move0 pa (tl ++ [pe,pc]) bd'
                     >>= applyOpW [up, up, lft, dwn, dwn, rgt, up]
  else if pos3 == pd then move0 pb (tl ++ [pd,pf]) bd'
                          >>= applyOpW [up, up, rgt, dwn, dwn, lft, up]
  else if pos3 == pc then move0 pb (tl ++ [pc,pa]) bd'
                          >>= applyOpW [rgt, up]
  else if pos3 == pb then move0 pa (tl ++ [pb,pd]) bd'
                          >>= applyOpW [rgt, up]
  else -- if pos3 == pa then
       return bd'


move34' :: Board -> [(Int,Int)] -> Int -> Int -> Writer [OP] Board
move34' bd frame m m' = do
  let [pa, pb, pc, pd, pe, pf] = frame
  let tl = [(0,3),(1,3),(0,2),(1,2)]
  let pos3 = findPos m bd
  let pos4 = findPos m' bd
  let greatPos34 p3 p4
         | (p3,p4)==(pe,pc) = True
         | (p3,p4)==(pd,pf) = True
         | (p3,p4)==(pb,pd) = True
         | (p3,p4)==(pa,pb) = True
         | (p3,p4)==(pc,pa) = True
         | otherwise = False
  if elem pos3 frame && elem pos4 frame then
       if greatPos34 pos3 pos4 then return bd
       else moveTile m (3,0) tl bd -- "away from the frame"
              >>= moveTile m' pc tl
              >>= moveTile m pe (tl ++ [pc])
  else if elem pos3 frame && not (elem pos4 frame) then
       moveTile m pd tl bd >>= moveTile m' pf (tl ++ [pd])
  else moveTile m' pc tl bd >>= moveTile m pe (tl ++ [pc])


move913 :: Board -> Writer [OP] Board
move913 bd = move34x bd [(0,1),(0,0),(1,1),(1,0),(2,1),(2,0)] S913 9 13


move78 :: Board -> Writer [OP] Board
move78 bd = do
  tell [S78]
  if findPos 7 bd == (2,2) && findPos 8 bd == (3,2)
  then return bd
  else do let tl = [(0,0),(0,1),(1,2),(2,3),(3,3)]
          bd' <- moveTile 7 (1,1) tl bd
          do if findPos 8 bd' == (1,0) then
               moveTile 8 (2,2) tl bd' -- without restriction
                >>= moveTile 7 (1,1) tl -- try again
             else return bd'
           >>= moveTile 8 (2,2) (tl ++ [(1,1)])
           >>= move0 (2,1) (tl ++ [(1,1),(2,2)])
           >>= applyOpW [R, U, L, L, D, D, R, U]


move1014 :: Board -> Writer [OP] Board
move1014 bd = do
  tell [S1014]
  if findPos 10 bd == (1,1) && findPos 14 bd == (1,0) then return bd
  else do
    let tl = [(0,0),(0,1),(1,2),(2,2),(2,3)]
    bd14 <- moveTile 14 (1,1) tl bd
    let pos10 = findPos 10 bd14
    let pos0 = findPos 0 bd14
    if pos10 == (1,0) then
      moveTile 10 (2,0) (tl ++ [(1,1)]) bd14 -- (1,0):0
        >>= move0 (3,1) (tl ++ [(2,0)])
        >>= moveTile 10 (3,1) (tl ++ [(1,0)])
        >>= moveTile 14 (2,1) (tl ++ [(3,1)])
        >>= move0 (1,0) (tl ++ [(2,1)])
        >>= applyOpW [D, L, L, U, R, R, D, L]
    else if pos0 == (1,0) then
      if pos10 == (2,1) then applyOpW [D, L] bd14
      else applyOpW [D, L] bd14
             >>= moveTile 10 (3,1) (tl ++ [(1,1)])
             >>= moveTile 14 (2,1) (tl ++ [(3,0),(3,1)])
             >>= move0 (1,1) (tl ++ [(2,1)])
             >>= applyOpW [L, L, U, R, R, D, L]
    else moveTile 10 (2,1) tl bd14
            >>= move0 (1,0) [(1,1),(2,1)]
            >>= applyOpW [D, L]


move1115 :: Board -> Writer [OP] Board
move1115 bd = do
  tell [S1115]
  let tl = [(1,0),(1,1),(2,2),(3,2)]
  moveTile 11 (2,1) tl bd
    >>= moveTile 12 (3,1) tl
    >>= moveTile 15 (2,0) tl


solve :: Board -> Writer [OP] Board
solve bd = return bd
  >>= move1 >>= move34 >>= move913 >>= move78 >>= move1014 >>= move1115


printSol :: [OP] -> IO ()
printSol [] = putStrLn ""
printSol (h:t) = do
  printPart h
  printSol t
    where printPart h
            | h == S12 = putStrLn "--[1,2]----------------------------"
            | h == S34 = putStrLn "\n--[3,4]----------------------------"
            | h == S56 = putStrLn "\n--[5,6]----------------------------"
            | h == S913 = putStrLn "\n--[9,13]---------------------------"
            | h == S78 = putStrLn "\n--[7,8]----------------------------"
            | h == S1014 = putStrLn "\n--[10,14]--------------------------"
            | h == S1115 = putStrLn "\n--[11,12,15]-----------------------"
            | otherwise = putStr $ " " ++ show h


printBoard :: Board -> IO ()
printBoard bd = do
  mapM_ (putStrLn.concat.(map f)) $ reverse bd
   where f 0 = "[  ]"
         f 1 = "[ 1]"
         f 2 = "[ 2]"
         f 3 = "[ 3]"
         f 4 = "[ 4]"
         f 5 = "[ 5]"
         f 6 = "[ 6]"
         f 7 = "[ 7]"
         f 8 = "[ 8]"
         f 9 = "[ 9]"
         f 10 = "[10]"
         f 11 = "[11]"
         f 12 = "[12]"
         f 13 = "[13]"
         f 14 = "[14]"
         f 15 = "[15]"


main :: IO ()
main = do
  putStrLn $ "Input a scrambled pattern:"
  ins <- getLine
  let bd = read ins :: Board
  putStrLn $ "Initial state:"
  printBoard $ bd
  let (bd', ops) = runWriter $ solve bd
  putStrLn $ "Solution: "
  printSol $ ops
  putStrLn $ "Final state:"
  printBoard $ bd'
  
