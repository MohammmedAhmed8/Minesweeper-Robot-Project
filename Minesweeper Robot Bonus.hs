type Cell = (Int, Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)

up :: MyState -> MyState
down :: MyState -> MyState
left :: MyState -> MyState
right :: MyState -> MyState

up (S (x, y) minesLoc action oldState) = S (x - 1, y) minesLoc "up" (S (x, y) minesLoc action oldState)
down (S (x, y) minesLoc action oldState) =  S (x + 1, y) minesLoc "down" (S (x, y) minesLoc action oldState)
left (S (x, y) minesLoc action oldState) = S (x, y - 1) minesLoc "left" (S (x, y) minesLoc action oldState)
right (S (x, y) minesLoc action oldState) = S (x, y + 1) minesLoc "right" (S (x, y) minesLoc action oldState)

collect :: MyState -> MyState
collect (S location minesLoc action oldState) = if (elem location minesLoc) then S location (filter (/= location) minesLoc) "collect" (S location minesLoc action oldState)
												else Null
isGoal :: MyState -> Bool
isGoal (S location minesLoc action oldState) = minesLoc == []

constructSolution :: MyState -> [String]
constructSolution Null = []
constructSolution (S location minesLoc action oldState) = if (action /= "") then constructSolution oldState ++ [action] else []

formState :: Cell -> [Cell] -> MyState
formState c l = S c l "" Null

getDistance :: Cell -> Cell -> Int
getDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
					  
insertPointSorted :: Cell -> Cell -> [Cell] -> [Cell]
insertPointSorted point c [] = [c]
insertPointSorted point c (h:t) = if (getDistance c point) > (getDistance h point) then (h: (insertPointSorted point c t)) else (c:h:t) 

sortPoints :: Cell -> [Cell] -> [Cell]
sortPoints point [] = []
sortPoints point (h:t) = insertPointSorted point h (sortPoints point t)
									  
pointReacher :: MyState -> MyState
pointReacher (S (x1, y1) ((x2, y2):t) action oldState)
											   | x1 > x2  = up (S (x1, y1) ((x2, y2):t) action oldState)
											   | x1 < x2 = down (S (x1, y1) ((x2, y2):t) action oldState)
											   | y1 > y2 = left (S (x1, y1) ((x2, y2):t) action oldState)
											   | y1 < y2 = right (S (x1, y1) ((x2, y2):t) action oldState)
											   | (x1 == x2 && y1 == y2) = collect (S (x1, y1) ((x2, y2):t) action oldState)
			
pointsReacher :: MyState -> MyState
pointsReacher (S c l action oldState) = if(isGoal (S c l action oldState)) then (S c l action oldState)
											  else pointsReacher (pointReacher (S c (sortPoints c l) action oldState))	
solve :: Cell -> [Cell] -> [String]
solve (x,y) l = if(x < 0 || y < 0) then error "Locations entered are out of Range !" else constructSolution(pointsReacher (formState (x,y) l))
























