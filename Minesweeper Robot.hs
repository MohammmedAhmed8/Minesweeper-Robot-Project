type Cell = (Int, Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)

up :: MyState -> MyState
down :: MyState -> MyState
left :: MyState -> MyState
right :: MyState -> MyState

up (S (x, y) minesLoc action oldState) = if(x > 0) then S (x - 1, y) minesLoc "up" (S (x, y) minesLoc action oldState) else Null
down (S (x, y) minesLoc action oldState) = if(x < 3) then S (x + 1, y) minesLoc "down" (S (x, y) minesLoc action oldState) else Null
left (S (x, y) minesLoc action oldState) = if(y > 0) then S (x, y - 1) minesLoc "left" (S (x, y) minesLoc action oldState) else Null
right (S (x, y) minesLoc action oldState) = if(y < 3) then S (x, y + 1) minesLoc "right" (S (x, y) minesLoc action oldState) else Null

collect :: MyState -> MyState
collect (S location minesLoc action oldState) = if (elem location minesLoc) then S location (filter (/= location) minesLoc) "collect" (S location minesLoc action oldState)
												else Null

nextMyStates :: MyState -> [MyState]
nextMyStates my_state = filter (/= Null) [up my_state, down my_state, left my_state, right my_state, collect my_state] 

getState :: String -> [MyState] -> MyState
getState s [] = Null
getState s ((S c l action state):t) = if(s == action) then (S c l action state)	else getState s t	

getDistance :: Cell -> Cell -> Int
getDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

insertPointSorted :: Cell -> Cell -> [Cell] -> [Cell]
insertPointSorted point c [] = [c]
insertPointSorted point c (h:t) = if (getDistance c point) > (getDistance h point) then (h: (insertPointSorted point c t)) else (c:h:t) 

sortPoints :: Cell -> [Cell] -> [Cell]
sortPoints point [] = []
sortPoints point (h:t) = insertPointSorted point h (sortPoints point t)

accurateState :: MyState -> MyState
accurateState (S (x1, y1) ((x2, y2):t) a s)
									   | x1 > x2 = getState "up" (nextMyStates (S (x1, y1) ((x2, y2):t) a s))
									   | x1 < x2 = getState "down" (nextMyStates (S (x1, y1) ((x2, y2):t) a s))
									   | y1 > y2 = getState "left" (nextMyStates (S (x1, y1) ((x2, y2):t) a s))
									   | y1 < y2 = getState "right" (nextMyStates (S (x1, y1) ((x2, y2):t) a s))
									   | (x1 == x2 && y1 == y2) = getState "collect" (nextMyStates (S (x1, y1) ((x2, y2):t) a s))
									   
isGoal :: MyState -> Bool
isGoal (S location minesLoc action oldState) = minesLoc == []

search :: [MyState] -> MyState
search (h:t) = if(isGoal h) then h else search(t ++ nextMyStates h)

constructSolution :: MyState -> [String]
constructSolution Null = []
constructSolution (S location minesLoc action oldState) = if (action /= "") then constructSolution oldState ++ [action] else []

getValidStates :: MyState -> [MyState]
getValidStates (S c l a s) = if(isGoal (S c l a s)) then [(S c l a s)] else getValidStates (accurateState (S c (sortPoints c l) a s))

formState :: Cell -> [Cell] -> MyState
formState c l = S c l "" Null

solve :: Cell -> [Cell] -> [String]
solve (x,y) l = if(x > 3 || x < 0 || y > 3 || y < 0) then error "Locations entered are out of Range !" else constructSolution(search(getValidStates (formState (x,y) l)))


