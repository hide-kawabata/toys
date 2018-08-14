{--------------------------------------------
  Tower of Hanoi
  Copyright (C) 2018 Hideyuki Kawabata

*Main> start 4
Tws {tl = [1,2,3,4], tc = [], tr = []}
*Main> goal 4
Tws {tl = [], tc = [], tr = [1,2,3,4]}
*Main> solve (start 4) (goal 4)
[LC,LR,CR,LC,RL,RC,LC,LR,CR,CL,RL,CR,LC,LR,CR]

  -- naive version
 --------------------------------------------}

import Control.Monad

data OP = LC | LR | CR | RC | RL | CL deriving (Eq, Ord, Show, Read)
data Tws = Tws { tl :: [Int]
               , tc :: [Int]
               , tr :: [Int]}
         deriving (Eq, Ord, Show, Read)

start, goal :: Int -> Tws
start n = Tws { tl = [1..n]
              , tc = []
              , tr = []}
goal n = Tws { tl = []
             , tc = []
             , tr = [1..n]}

printTws tws = do
  putStrLn $ show $ tl tws
  putStrLn $ show $ tc tws
  putStrLn $ show $ tr tws

applyOp :: [OP] -> Tws -> Tws
applyOp [] f = f
applyOp (x:xs) f = applyOp xs $ move x f

movable :: [Int] -> [Int] -> Bool
movable [] _ = False
movable (h:t) [] = True
movable (h:t) (h':t') = h < h'

move :: OP -> Tws -> Tws
move op t
 | op == LC && movable ll lc = Tws { tl = tail ll, tc = head ll:lc, tr = lr }
 | op == LR && movable ll lr = Tws { tl = tail ll, tc = lc, tr = head ll:lr }
 | op == CR && movable lc lr = Tws { tl = ll, tc = tail lc, tr = head lc:lr }
 | op == RC && movable lr lc = Tws { tl = ll, tc = head lr:lc, tr = tail lr }
 | op == RL && movable lr ll = Tws { tl = head lr:ll, tc = lc, tr = tail lr }
 | op == CL && movable lc ll = Tws { tl = head lc:ll, tc = tail lc, tr = lr }
 | otherwise = t
  where ll = tl t
        lc = tc t
        lr = tr t

moveDisks :: Int -> [OP] -> Tws -> [[OP]]
moveDisks 0 cur scrambled = return cur
moveDisks n cur scrambled = do
  cur' <- moveOneDisk cur scrambled
  moveDisks (n-1) cur' scrambled

moveOneDisk :: [OP] -> Tws -> [[OP]]
moveOneDisk cur scrambled = do
  op <- [LR, LC, CR, RC, RL, CL]
  guard $ check op cur scrambled
  return $ cur ++ [op]

check :: OP -> [OP] -> Tws -> Bool
check op l frm = iter_check new_frm l frm
  where new_frm = applyOp (l ++ [op]) frm 

iter_check :: Tws -> [OP] -> Tws -> Bool
iter_check new_frm [] frm = new_frm /= frm
iter_check new_frm (h:t) frm 
  | new_frm == frm = False
  | otherwise = iter_check new_frm t (move h frm)

solveN :: Int -> Tws -> Tws -> [[OP]]
solveN n scrambled goal = do
  ops <- moveDisks n [] scrambled
  guard $ applyOp ops scrambled == goal
  return ops

solve :: Tws -> Tws -> [OP]
solve scrambled goal 
  = head $ head $ dropWhile (\l -> l == []) $
    map (\n -> solveN n scrambled goal) [1..]
