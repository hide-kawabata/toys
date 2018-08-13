{--------------------------------------------
  N-Queen Solver
  Copyright (C) 2018 Hideyuki Kawabata

  -- naive version
 --------------------------------------------}

import Control.Monad

solveNQ :: Int -> [[(Int, Int)]]
solveNQ n = do
  l <- putPieces n n []
  return l

putPieces :: Int -> Int -> [(Int, Int)] -> [[(Int, Int)]]
putPieces 0 n cur = return cur
putPieces m n cur = do
  cur' <- putOnePiece n cur
  putPieces (m-1) n cur'

putOnePiece :: Int -> [(Int, Int)] -> [[(Int, Int)]]
putOnePiece n cur = do
  x <- [1..n]
  y <- [1..n]
  guard $ check (x, y) cur
  return $ (x, y):cur

check :: (Int, Int) -> [(Int, Int)] -> Bool
check (x, y) [] = True
check (x, y) ((x', y'):t)
  | x == x' = False
  | y == y' = False
  | abs (x - x') == abs (y - y') = False
  | otherwise = check (x, y) t
  
main :: IO ()
main = do
  putStr $ "Input size : "
  s <- getLine
  let r = solveNQ (read s :: Int)
--  mapM_ (putStrLn.show) r
  putStrLn $ show $ take 1 r
