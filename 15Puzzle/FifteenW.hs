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

move1 :: Board -> Writer [OP] Board
move1 bd = do
  tell [S12]
  bd1 <- move1' bd 1 (0,3) []
  bd2 <- move1' bd1 2 (1,3) [(0,3)]
  tell [S56]
  bd3 <- move1' bd2 5 (0,2) [(0,3),(1,3)]
  bd4 <- move1' bd3 6 (1,2) [(0,3),(1,3),(0,2)]
  return bd4

move1' :: Board -> Int -> Point -> [Point] -> Writer [OP] Board
move1' bd m gl tl = do
  let s = findPos m bd
  if s == gl then return bd
  else do bd' <- move1'' bd s gl tl
          bd'' <- move1' bd' m gl tl
          return bd''

move1'' :: Board -> Point -> Point -> [Point] -> Writer [OP] Board
move1'' bd s gl tl = do 
  let z = findPos 0 bd
  let t = head $ drop 1 $ reverse $ findOnePath s gl tl -- 0's target
  let p = findOnePath z t (s:tl)
  let ops = pathToOp p
  let p' = pathToOp [s, t] -- additional move
  let ops' = p' ++ ops
  let bd' = applyOp (reverse ops') bd
  writer (bd', reverse ops')


move0 :: Board -> Point -> [Point] -> Writer [OP] Board
move0 bd gl tl = do
  let z = findPos 0 bd
  let p = findOnePath z gl tl
  let revops = reverse $ pathToOp p
  let bd' = applyOp revops bd
  writer (bd', revops)


applyOpW :: [OP] -> Board -> Writer [OP] Board
applyOpW ops bd = writer (applyOp ops bd, ops)


move34 :: Board -> Writer [OP] Board
move34 bd = do
  tell [S34]
  bd' <- move34' bd
  let tl = [(0,2),(1,2),(1,3)]
  case (findPos 3 bd') of
    (2,1) -> do bd21 <- move0 bd' (2,3) $ tl ++ [(2,1),(2,2)]
                applyOpW [U, U, L, D, D, R, U] bd21
    (3,2) -> do bd32 <- move0 bd' (3,3) $ tl ++ [(3,2),(3,1)]
                applyOpW [U, U, R, D, D, L, U] bd32
    (2,2) -> do bd22 <- move0 bd' (3,3) $ tl ++ [(2,2),(2,3)]
                applyOpW [R, U] bd22
    (3,3) -> do bd33 <- move0 bd' (2,3) $ tl ++ [(3,3),(3,2)]
                applyOpW [R, U] bd33
    (2,3) -> return bd'

move34' :: Board -> Writer [OP] Board
move34' bd = do
  let frame = [(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
  let tl = [(0,3),(1,3),(0,2),(1,2)]
  let pos3 = findPos 3 bd
  let pos4 = findPos 4 bd
  if elem pos3 frame && elem pos4 frame then
       if greatPos34 pos3 pos4 then return bd
       else 
       do bd'' <- move1' bd 3 (3,0) tl
          bd' <- move1' bd'' 4 (2,2) tl
          bd3 <- move1' bd' 3 (2,1) $ tl ++ [(2,2)]
          return bd3
  else if not (elem pos3 frame) && elem pos4 frame then
       do bd' <- move1' bd 4 (2,2) tl
          bd3 <- move1' bd' 3 (2,1) $ tl ++ [(2,2)]
          return bd3
  else if elem pos3 frame && not (elem pos4 frame) then
       do bd' <- move1' bd 3 (3,2) tl
          bd3 <- move1' bd' 4 (3,1) $ tl ++ [(3,2)]
          return bd3
  else do bd' <- move1' bd 4 (2,2) tl
          bd3 <- move1' bd' 3 (2,1) $ tl ++ [(2,2)]
          return bd3
 where greatPos34 p3 p4 = case (p3, p4) of
                          ((2,1), (2,2)) -> True
                          ((3,2), (3,1)) -> True
                          ((3,3), (3,2)) -> True
                          ((2,3), (3,3)) -> True
                          ((2,2), (2,3)) -> True
                          _ -> False


move913 :: Board -> Writer [OP] Board
move913 bd = do
  tell [S913]
  bd' <- move913' bd
  let tl = [(0,2),(1,2),(2,2),(3,2)]
  case (findPos 9 bd') of
    (2,1) -> do bd21 <- move0 bd' (0,1) $ tl ++ [(2,1),(1,1)]
                applyOpW [L, L, U, R, R, D, L] bd21
    (1,0) -> do bd10 <- move0 bd' (0,0) $ tl ++ [(1,0),(2,0)]
                applyOpW [L, L, D, R, R, U, L] bd10
    (0,0) -> do bd00 <- move0 bd' (0,1) $ tl ++ [(0,0),(1,0)]
                applyOpW [U, L] bd00
    (1,1) -> do bd11 <- move0 bd' (0,0) $ tl ++ [(1,1),(0,1)]
                applyOpW [D, L] bd11
    (0,1) -> return bd'


move913' :: Board -> Writer [OP] Board
move913' bd = do
  let frame = [(0,0),(0,1),(1,0),(1,1),(2,0),(2,1)]
  let tl = [(0,3),(1,3),(0,2),(1,2)]
  let pos9 = findPos 9 bd
  let pos13 = findPos 13 bd
  if elem pos9 frame && elem pos13 frame then
       if greatPos913 pos9 pos13 then return bd
       else 
       do bd'' <- move1' bd 9 (3,0) tl
          bd' <- move1' bd'' 13 (1,1) tl
          bd3 <- move1' bd' 9 (2,1) $ tl ++ [(1,1)]
          return bd3
  else if not (elem pos9 frame) && elem pos13 frame then
       do bd' <- move1' bd 13 (1,1) tl
          bd3 <- move1' bd' 9 (2,1) $ tl ++ [(1,1)]
          return bd3
  else if elem pos9 frame && not (elem pos13 frame) then
       do bd' <- move1' bd 9 (1,0) tl
          bd3 <- move1' bd' 13 (2,0) $ tl ++ [(1,0)]
          return bd3
  else do bd' <- move1' bd 13 (1,1) tl
          bd3 <- move1' bd' 9 (2,1) $ tl ++ [(1,1)]
          return bd3
 where greatPos913 p9 p13 = case (p9, p13) of
                            ((2,1), (1,1)) -> True
                            ((1,0), (2,0)) -> True
                            ((0,0), (1,0)) -> True
                            ((0,1), (0,0)) -> True
                            ((1,1), (0,1)) -> True
                            _ -> False


move78 :: Board -> Writer [OP] Board
move78 bd = do
  tell [S78]
  if findPos 7 bd == (2,2) && findPos 8 bd == (3,2)
  then return bd
  else do let tl = [(0,0),(0,1),(1,2),(2,3),(3,3)]
          bd7 <- move1' bd 7 (1,1) tl
          bd8 <- move1' bd7 8 (2,2) $ tl ++ [(1,1)]
          bdz <- move0 bd8 (2,1) $ tl ++ [(1,1),(2,2)]
          applyOpW [R, U, L, L, D, D, R, U] bdz


move1014 :: Board -> Writer [OP] Board
move1014 bd = do
  tell [S1014]
  if findPos 10 bd == (1,1) && findPos 14 bd == (1,0)
  then return bd
  else do let tl = [(0,0),(0,1),(1,2),(2,2),(2,3)]
          bd14 <- move1' bd 14 (1,1) tl
          if findPos 10 bd14 == (1,0) then do
            bd14' <- move1' bd14 10 (2,0) $ tl ++ [(1,1)] -- (1,0):0
            applyOpW [D, L, L, U, R, D, L, U, R, R, D,
                      L, U, R, D, L, L, U, R, R, D, L] bd14'
          else if findPos 0 bd14 == (1,0) then do
            if findPos 10 bd14 == (2,1) then do
              applyOpW [D, L] bd14
            else do
              bd2 <- applyOpW [D, L] bd14
              bd14' <- move1' bd2 10 (3,1) $ tl ++ [(1,1)]
              applyOpW [U, R, D, L, U, R, D, L, L, U, R, R, D, L] bd14'
          else do
            bd10 <- move1' bd14 10 (2,1) tl
            bdz <- move0 bd10 (1,0) [(1,1),(2,1)]
            applyOpW [D, L] bdz


move1115 :: Board -> Writer [OP] Board
move1115 bd = do
  tell [S1115]
  let tl = [(1,0),(1,1),(2,2),(3,2)]
  bd11 <- move1' bd 11 (2,1) tl
  bd12 <- move1' bd11 12 (3,1) tl
  bd15 <- move1' bd12 15 (2,0) tl
  return bd15


solve :: Board -> Writer [OP] Board
solve bd = do
  bd1256 <- move1 bd
  bd34 <- move34 bd1256
  bd913 <- move913 bd34
  bd78 <- move78 bd913
  bd1014 <- move1014 bd78
  move1115 bd1014


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
  
