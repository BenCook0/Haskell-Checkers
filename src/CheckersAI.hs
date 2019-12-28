module CheckersAI where

import Moves
import ApplyMove
import GameLogic

--AI implementation by Ben Cook 30037563

--black heurisitc presented in class
black_heuristic :: GameState -> Int 
black_heuristic (GameState bp rp bk rk s m) = ((length bp) - (length rp)) + 2*((length bk) - (length rk))

--black AI
black_ai:: GameState -> Move
black_ai g 
  |moves g == [] = []
  |otherwise = applyBest g (fst(blackB_aiCalc g 4))

--red heuristic presented in class
red_heuristic :: GameState -> Int
red_heuristic (GameState bp rp bk rk s m) = ((length rp) - (length bp)) + 2*((length rk) - (length bk))

--red AI
red_ai:: GameState -> Move
red_ai g 
  |moves g == [] = []
  |otherwise = applyBest g (fst(redR_aiCalc g 4))

--calculation for black moves in the black AI
blackB_aiCalc:: GameState -> Int -> (GameState,Int)
blackB_aiCalc g 0 = (g,(black_heuristic g))        --base case, base recursion depth
blackB_aiCalc (GameState bp rp bk rk GameOver m) _ = ((GameState bp rp bk rk GameOver m),(black_heuristic (GameState bp rp bk rk GameOver m))) --if the game finished
blackB_aiCalc g x = bestH (zip c (map snd d))
  where
    b = moves g
    c = applyAll b g
    d = mapApply redB_aiCalc c (x-1)

--calculation for red moves in black AI
redB_aiCalc:: GameState -> Int -> (GameState,Int)
redB_aiCalc g 0 = (g,(red_heuristic g))        --base case, base recursion depth
redB_aiCalc (GameState bp rp bk rk GameOver m) _ = ((GameState bp rp bk rk GameOver m),(red_heuristic (GameState bp rp bk rk GameOver m)))
redB_aiCalc g x = worstH (zip c (map snd d))
  where
    b = moves g
    c = applyAll b g
    d = mapApply blackB_aiCalc c (x-1)

--calculation for black moves in red AI
blackR_aiCalc:: GameState -> Int -> (GameState,Int)
blackR_aiCalc g 0 = (g,(black_heuristic g))        --base case, base recursion depth
blackR_aiCalc (GameState bp rp bk rk GameOver m) _ = ((GameState bp rp bk rk GameOver m),(black_heuristic (GameState bp rp bk rk GameOver m))) --if the game finished
blackR_aiCalc g x = worstH (zip c (map snd d))
  where
    b = moves g
    c = applyAll b g
    d = mapApply redR_aiCalc c (x-1)

--calculation for red moves in black AI
redR_aiCalc:: GameState -> Int -> (GameState,Int)
redR_aiCalc g 0 = (g,(red_heuristic g))        --base case, base recursion depth
redR_aiCalc (GameState bp rp bk rk GameOver m) _ = ((GameState bp rp bk rk GameOver m),(red_heuristic (GameState bp rp bk rk GameOver m)))
redR_aiCalc g x = bestH (zip c (map snd d))
  where
    b = moves g
    c = applyAll b g
    d = mapApply blackR_aiCalc c (x-1)

--take an old gamestate and a new gamestate, and finds the move that must be applied to old to get new
applyBest :: GameState -> GameState -> Move
applyBest old new = findBestMove (applyAll (moves old) old) (moves old)
    where 
      findBestMove :: [GameState] -> [Move] -> Move
      findBestMove [] [] = error "never executes"
      findBestMove (x:xs) (y:ys)
        |(apply_move y old) == new = y
        |otherwise = findBestMove xs ys
  
--calculates the best (highest) heuristic given a list of pairs of gamestates,ints
bestH:: [(GameState,Int)] -> (GameState,Int)
bestH [] = error "should never happen"
bestH ((g,i):[]) = (g,i)
bestH ((g,i2):xs) = sndmax (g,i2) (bestH xs)

--calculates the worst (lowest) heuristic given a list of pairs of gamestates,ints
worstH:: [(GameState,Int)] -> (GameState,Int)
worstH [] = error "should never happen"
worstH ((g,i):[]) = (g,i)
worstH ((g,i2):xs) = sndmin (g,i2) (worstH xs)

--applies a function over a list of Gamestates and an int (used for the mutual recursion)
mapApply:: (GameState -> Int -> (GameState,Int)) -> [GameState] -> Int -> [(GameState,Int)]
mapApply (f) xs i = [g|x <- xs, let g = (f x i)]

--finds the smaller of two elements based on the second value in the tuple
sndmin:: (GameState,Int) -> (GameState,Int) -> (GameState,Int)
sndmin (x,i) (y,i2)
  |i >= i2 = (y,i2)
  |otherwise = (x,i)

--finds the smaller of two elements based on the second value in the tuple
sndmax:: (GameState,Int) -> (GameState,Int) -> (GameState,Int)
sndmax (x,i) (y,i2)
  |i >= i2 = (x,i)
  |otherwise = (y,i2)

--top value used for pruning
top:: Int
top = 3000

--bot value used for pruning
bot:: Int
bot = (-3000)

--takes in a list of moves and a gamestate, returns a list of pairs of moves with the resulting gamestate if said move was applied
applyAll :: [Move] -> GameState -> [GameState]
applyAll [] g = [g]
applyAll ms g = [b| a <- ms, let b = (apply_move a g)]



