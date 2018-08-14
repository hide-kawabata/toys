{--------------------------------------------
  15 Puzzle Solver
  Copyright (C) 2018 Hideyuki Kawabata


*Main> printFrame goal
(T01,T02,T03,T04)
(T05,T06,T07,T08)
(T09,T10,T11,T12)
(T13,T14,T15,SPC)
*Main> printFrame $ applyOp [D,D,R,U,R,R,D,D] goal
(SPC,T02,T03,T04)
(T01,T06,T11,T07)
(T05,T09,T10,T08)
(T13,T14,T15,T12)
*Main> solve (applyOp [D,D,R,U,R,R,D,D] goal) goal
[U,U,L,L,D,L,U,U]

  -- naive version
 --------------------------------------------}

import Control.Monad

data OP = R | L | U | D deriving (Eq, Ord, Show, Read)
data TL = T01 | T02 | T03 | T04 | T05 
        | T06 | T07 | T08 | T09 | T10
        | T11 | T12 | T13 | T14 | T15
        | SPC | DMY
        deriving (Eq, Ord, Show, Read)
data Frm = Frm { l1 :: (TL, TL, TL, TL)
               , l2 :: (TL, TL, TL, TL)
               , l3 :: (TL, TL, TL, TL)
               , l4 :: (TL, TL, TL, TL)}
         deriving (Eq, Ord, Show, Read)

goal :: Frm
goal = Frm { l1 = (T01, T02, T03, T04)
           , l2 = (T05, T06, T07, T08)
           , l3 = (T09, T10, T11, T12)
           , l4 = (T13, T14, T15, SPC)}

printFrame frm = do
  putStrLn $ show $ l1 frm
  putStrLn $ show $ l2 frm
  putStrLn $ show $ l3 frm
  putStrLn $ show $ l4 frm

applyOp :: [OP] -> Frm -> Frm
applyOp [] f = f
applyOp (x:xs) f = applyOp xs $ move x f

move :: OP -> Frm -> Frm
move R f = Frm { l1 = moveRL R $ l1 f
               , l2 = moveRL R $ l2 f
               , l3 = moveRL R $ l3 f
               , l4 = moveRL R $ l4 f}
move L f = Frm { l1 = moveRL L $ l1 f
               , l2 = moveRL L $ l2 f
               , l3 = moveRL L $ l3 f
               , l4 = moveRL L $ l4 f}
move U f = Frm { l1 = moveUD U (DMY, DMY, DMY, DMY) (l1 f) (l2 f)
               , l2 = moveUD U (l1 f) (l2 f) (l3 f)
               , l3 = moveUD U (l2 f) (l3 f) (l4 f)
               , l4 = moveUD U (l3 f) (l4 f) (DMY, DMY, DMY, DMY)}
move D f = Frm { l1 = moveUD D (DMY, DMY, DMY, DMY) (l1 f) (l2 f)
               , l2 = moveUD D (l1 f) (l2 f) (l3 f)
               , l3 = moveUD D (l2 f) (l3 f) (l4 f)
               , l4 = moveUD D (l3 f) (l4 f) (DMY, DMY, DMY, DMY)}

moveRL :: OP -> (TL, TL, TL, TL) -> (TL, TL, TL, TL)
moveRL R (x, y, z, SPC) = (x, y, SPC, z)
moveRL R (x, y, SPC, z) = (x, SPC, y, z)
moveRL R (x, SPC, y, z) = (SPC, x, y, z)
moveRL R q = q
moveRL L (SPC, x, y, z) = (x, SPC, y, z)
moveRL L (x, SPC, y, z) = (x, y, SPC, z)
moveRL L (x, y, SPC, z) = (x, y, z, SPC)
moveRL L q = q

moveUD :: OP -> (TL, TL, TL, TL) -> (TL, TL, TL, TL) -> (TL, TL, TL, TL)
   -> (TL, TL, TL, TL)
moveUD U (a, b, c, d) (e, f, g, h) (i, j, k, l) 
  | a == SPC = (SPC, f, g, h)
  | b == SPC = (e, SPC, g, h)
  | c == SPC = (e, f, SPC, h)
  | d == SPC = (e, f, g, SPC)
  | e == SPC && i /= DMY = (i, f, g, h)
  | f == SPC && j /= DMY = (e, j, g, h)
  | g == SPC && k /= DMY = (e, f, k, h)
  | h == SPC && l /= DMY = (e, f, g, l)
  | otherwise = (e, f, g, h)
moveUD D (a, b, c, d) (e, f, g, h) (i, j, k, l) 
  | e == SPC && a /= DMY = (a, f, g, h)
  | f == SPC && b /= DMY = (e, b, g, h)
  | g == SPC && c /= DMY = (e, f, c, h)
  | h == SPC && d /= DMY = (e, f, g, d)
  | i == SPC = (SPC, f, g, h)
  | j == SPC = (e, SPC, g, h)
  | k == SPC = (e, f, SPC, h)
  | l == SPC = (e, f, g, SPC)
  | otherwise = (e, f, g, h)

moveTiles :: Int -> [OP] -> Frm -> [[OP]]
moveTiles 0 cur scrambled = return cur
moveTiles n cur scrambled = do
  cur' <- moveOneTile cur scrambled
  moveTiles (n-1) cur' scrambled

moveOneTile :: [OP] -> Frm -> [[OP]]
moveOneTile cur scrambled = do
  op <- [R, L, U, D]
  guard $ check op cur scrambled
  return $ cur ++ [op]

check :: OP -> [OP] -> Frm -> Bool
check op l frm = iter_check new_frm l frm
  where new_frm = applyOp (l ++ [op]) frm 

iter_check :: Frm -> [OP] -> Frm -> Bool
iter_check new_frm [] frm = new_frm /= frm
iter_check new_frm (h:t) frm 
  | new_frm == frm = False
  | otherwise = iter_check new_frm t (move h frm)

solveN :: Int -> Frm -> Frm -> [[OP]]
solveN n scrambled goal = do
  ops <- moveTiles n [] scrambled
  guard $ applyOp ops scrambled == goal
  return ops

solve :: Frm -> Frm -> [OP]
solve scrambled goal 
  = head $ head $ dropWhile (\l -> l == []) $
    map (\n -> solveN n scrambled goal) [1..]
