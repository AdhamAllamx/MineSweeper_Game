type Cell = (Int,Int) 
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

up :: MyState -> MyState
up (S (x,y) mines lastAction state)    | x == 0 = Null
                                       | otherwise = S (x-1,y) mines "up" (S (x,y) mines lastAction state) 
									   
down :: MyState -> MyState
down (S (x,y) mines lastAction state)  | x == 1000 = Null
									   | otherwise = S (x+1,y) mines "down" (S (x,y) mines lastAction state) 
									   
left :: MyState -> MyState
left (S (x,y) mines lastAction state)  | y == 0 = Null
									   | otherwise = S (x,y-1) mines "left" (S (x,y) mines lastAction state) 

right :: MyState -> MyState
right (S (x,y) mines lastAction state) | y == 1000 = Null
									   | otherwise = S (x,y+1) mines "right" (S (x,y) mines lastAction state) 

collect:: MyState -> MyState
collect	(S (x,y) mines lastAction state) | pairNotInList (x,y) mines = Null 	
                                         | otherwise = S (x,y) (removePairFromList(x,y) mines) "collect" (S (x,y) mines lastAction state)					   
										 
sorted :: Cell -> [Cell] -> [Cell]				   
sorted (x,y) [] = []					 
sorted (x,y) ((x1,y1):xs) =  sorted (x,y) (removelast (helpersort (x,y) ((x1,y1):xs))) ++ [ last1( helpersort (x,y) ((x1,y1):xs) )]
                                        						
search:: MyState -> MyState	
search (S (x,y) [] lastAction state) = 	(S (x,y) [] lastAction state)				     
search (S (x,y) ((x1,y1):m) lastAction state)     | x == x1 && y == y1 = search (collect (S (x,y) (sorted (x,y) ((x1,y1):m)) lastAction state))	
                                                  | x > x1  = search (up (S (x,y) (sorted (x,y) ((x1,y1):m)) lastAction state) )
                                                  | x < x1  = search (down (S (x,y) (sorted (x,y) ((x1,y1):m)) lastAction state) )
                                                  | x == x1 && y < y1  = search (right (S (x,y) (sorted (x,y) ((x1,y1):m)) lastAction state) )
                                                  | x == x1 && y > y1  = search (left (S (x,y) (sorted (x,y) ((x1,y1):m)) lastAction state) )											  

constructSolution:: MyState -> [String]
constructSolution  (S (x,y) mines "" Null) = []
constructSolution  (S (x,y) mines lastAction state) =  constructSolution state ++ [lastAction] 
                                                    
solve :: Cell -> [Cell] -> [String]                                                 
solve (x,y) mines = constructSolution (search  (S (x,y) (sorted (x,y) mines) "" Null)  )
	
-- below are some helper methods

getx (x,y) = x	
gety (x,y) = y

last1 [(x1,y1)] = (x1,y1)
last1 ((x1,y1):xs) = last1 xs

removelast [(x1,y1)] = []
removelast ((x1,y1):xs) = (x1,y1):removelast xs

toPositive x | x>=0  = x
             | otherwise = (-1)*x

helpersort :: Cell -> [Cell] -> [Cell]			 
helpersort (x,y) [] = []
helpersort (x,y) ((x1,y1):xs) = if ( (toPositive (getx (last1((x1,y1):xs)) - x)) + (toPositive (gety (last1((x1,y1):xs)) - y)))  >= (toPositive(x1-x) + toPositive(y1-y)) then (x1,y1):helpersort (x,y) (xs)
			              else  last1((x1,y1):xs): helpersort (x,y) (removelast((x1,y1):xs))
	
pairNotInList :: Cell -> [Cell] -> Bool										 
pairNotInList pair [] = True
pairNotInList pair (h:t) | pair == h = False
                         | otherwise = pairNotInList pair t 

removePairFromList :: Cell -> [Cell] -> [Cell]						 
removePairFromList pair (h:t) | pair == h = t
                              | otherwise = h : removePairFromList pair t						 

removeNullsFromList :: [MyState] -> [MyState]
removeNullsFromList [] = []
removeNullsFromList (h:t) | h == Null = removeNullsFromList t
						  | otherwise = (h : removeNullsFromList t)