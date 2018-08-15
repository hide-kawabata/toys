{--------------------------------------------
  Tower of Hanoi
  Copyright (C) 2018 Hideyuki Kawabata

*Main> solve 4
[LC,LR,CR,LC,RL,RC,LC,LR,CR,CL,RL,CR,LC,LR,CR]

  -- Tower of Hanoi-specific solution
 --------------------------------------------}

import Control.Monad

data OP = LC | LR | CR | RC | RL | CL deriving (Eq, Ord, Show, Read)
data Pos = L | R | C deriving (Eq, Ord, Show, Read)

solution :: Pos -> Pos -> Pos -> Int -> [OP]
solution L C R 0 = [LR]
solution L R C 0 = [LC]
solution C R L 0 = [CL]
solution C L R 0 = [CR]
solution R C L 0 = [RL]
solution R L C 0 = [RC]
solution L C R n = solution L R C (n-1) ++ [LR] ++ solution C L R (n-1)
solution L R C n = solution L C R (n-1) ++ [LC] ++ solution R L C (n-1)
solution R L C n = solution R C L (n-1) ++ [RC] ++ solution L R C (n-1)
solution R C L n = solution R L C (n-1) ++ [RL] ++ solution C R L (n-1)
solution C R L n = solution C L R (n-1) ++ [CL] ++ solution R C L (n-1)
solution C L R n = solution C R L (n-1) ++ [CR] ++ solution L C R (n-1)

solve :: Int -> [OP]
solve n = solution L C R (n-1)

main :: IO ()
main = do
  putStrLn $ "Input size: "
  s <- getLine
  let n = read s :: Int
  putStrLn $ show $ solve n
