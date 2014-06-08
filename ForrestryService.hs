-- Simulates a forrest
-- Challenge from: http://www.reddit.com/r/dailyprogrammer/comments/27h53e/662014_challenge_165_hard_simulated_ecology_the/

-- I'm getting this state monad thing, let's also make an animated gif!

import System.Environment
import System.Random
import System.IO
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans
import Codec.Picture.Gif
import Codec.Picture.Types

------------------ Some types we'll use ------------------

type Tree = Int				-- Age in months

type PossibleTree = Maybe Tree

type Coords = (Int, Int)

type MovesLeft = Int

type Lumberjack = (Coords, MovesLeft)

type Bear = (Coords, MovesLeft)

type Forrest = [[PossibleTree]]

type Frame = Image Pixel8

data ForrestState = ForrestState {
					forrest			:: Forrest,
					lumberjacks		:: [Lumberjack],
					bears			:: [Bear],
					month			:: Int,
					harvests		:: Int,
					yearHarvests	:: Int,
					sprouts			:: Int,
					matures			:: Int,
					elderly			:: Int,
					maulings		:: Int,
					yearMaulings	:: Int,
					randGen			:: StdGen,
					frames			:: [Frame],
					size			:: Int
				}

type ForrestFunction a = StateT ForrestState IO a

------------------ Static palette we'll use for everything ------------------

saplingRGB		= PixelRGB8 0x00 0xFF 0x00
matureRGB		= PixelRGB8 0x00 0xCC 0x00
elderlyRGB		= PixelRGB8 0x00 0x99 0x00
lumberjackRGB 	= PixelRGB8 0xCC 0x00 0x00
bearRGB			= PixelRGB8 0x66 0x33 0x00
emptyRGB		= PixelRGB8 0xFF 0xFF 0xFF

ourPallet = generateImage usePallet 6 1
	where
		usePallet x _ = [saplingRGB, matureRGB, elderlyRGB, lumberjackRGB, bearRGB, emptyRGB] !! x

saplingColor 	= 0
matureColor		= 1
elderlyColor	= 2
lumberjackColor	= 3
bearColor		= 4
emptyColor 		= 5

------------------ Functions to work on our types ------------------

isSapling :: PossibleTree -> Bool
isSapling (Just t)	= t <= 12
isSapling _			= False

isMature :: PossibleTree -> Bool
isMature (Just t)	= t > 12 && t <= 120
isMature _			= False

isElder :: PossibleTree -> Bool
isElder (Just t)	= t > 120
isElder _			= False

-- Check if the tree should generate a sapling this month
shouldSpawn :: PossibleTree -> ForrestFunction Bool
shouldSpawn t = do
					r <- gets randGen
					
					let (n, r') = randomR (1 :: Int, 10 :: Int) r
					
					modify (\s -> s{randGen = r'})
					
					case t of _
							| isNothing t	-> return False		-- Non-trees and saplings can't spawn
							| isSapling t	-> return False
							| isMature t	-> return $ n == 1	-- Mature have 10% chance at spawn each month
							| isElder t		-> return $ n <= 2	-- Elders have 20% chance at spawn each month
						
-- Given a tree at coords, get the coords it can spawn at
possibleSproutPoints :: Coords -> ForrestFunction [Coords]
possibleSproutPoints c = do
							t <- getTree c
					
							neighboringCoords <- neighboringCells c					-- Neighboring cells
							neighboringTrees <- mapM getTree neighboringCoords		-- Neighboring trees
							
							let pairs = zip neighboringCoords neighboringTrees		-- Match 'em together so we can filter
							let goodPairs = filter (isNothing . snd) pairs			-- Take only empty spots

							if isNothing t || isSapling t then
								return []
							else
								return $ map fst goodPairs							-- Return only the coords

-- Check if the simulation is over (4800 months or no trees left)
simulationOver :: ForrestFunction Bool
simulationOver = do
					m <- gets month
					(s, m, e) <- countTrees
					
					return $ (m >= 4800) || (s + m + e == 0)

-- Check if there is anywhere for trees to grow
freePlots :: ForrestFunction Bool
freePlots = do
				f <- gets forrest
				
				return $ any isNothing (concat f)

-- Get the coords of every tree in the forrest that can spawn
findSpawnableTrees :: ForrestFunction [Coords]
findSpawnableTrees = do
						f <- gets forrest
						
						let rowsWithYs		= addYCoords f								-- Turn [row] to [(row, Y-coord)]
						let allWithXCoords	= map addXCoords rowsWithYs					-- Turn each row to [(PossibleTree, x, y)]
						let asBigList		= concat allWithXCoords						-- Flatten that
						let onlySpawnable	= filter (\(t, _, _) -> isMature t || isElder t) asBigList	-- Keep only trees that can spawn
						
						return $ map (\(t, x, y) -> (x, y)) onlySpawnable				-- Stip out the tree from each tuple						
					where
						addYCoords f = zip f [0,1..]
						addXCoords (r, y) = zip3 r [0,1..] (repeat y)

-- Get the tree at the given spot
getTree :: Coords -> ForrestFunction PossibleTree
getTree (x, y) = do
					f <- gets forrest
					return $ f !! y !! x

-- Change a tree
setTree :: Coords -> PossibleTree -> ForrestFunction ()
setTree (x, y) t = do
						f <- gets forrest
					
						let (rowsBefore, rowToChange:rowsAfter)	= splitAt y f
						let (lineBefore, _:lineAfter)			= splitAt x rowToChange
						let updatedForrest = rowsBefore ++ (lineBefore ++ t:lineAfter):rowsAfter
					
						modify (\s -> s{forrest = updatedForrest})							
					where


-- Find the coords of all the neighboring cells
neighboringCells :: Coords -> ForrestFunction [Coords]
neighboringCells (x, y) = do
						s <- gets size
						
						let possibilities = [(x + i, y + j) | i <- [-1..1], j <- [-1..1], i /= 0 && j /= 0]

						return $ filter (\(x, y) -> x >= 0 && y >= 0 && x < s && y < s) possibilities

-- Count the number of trees in each state
countTrees :: ForrestFunction (Int, Int, Int)
countTrees = do
				f <- gets forrest

				return $ foldl countingFunc (0, 0, 0) (concat f)
			where
				countingFunc (s, m, e) t
									| isSapling t	= (s + 1, m, e)
									| isMature t	= (s, m + 1, e )
									| isElder t		= (s, m, e + 1)
									| isNothing t	= (s, m, e)

-- Increment the age of all the trees in the forrest
incrementTrees :: ForrestFunction ()
incrementTrees = do
					f <- gets forrest
					
					let updatedForrest = map updateRow f
					
					modify (\s -> s{forrest = updatedForrest})
				where
					updateTree (Nothing)	= Nothing
					updateTree (Just t)		= Just $ t + 1
					updateRow				= map updateTree

-- Increment the month number, return if we're in a new year
incrementMonth :: ForrestFunction Bool
incrementMonth = do
					m <- gets month
					
					modify (\s -> s{month = m + 1})
					
					return $ (m + 1) `mod` 12 == 0

-- Clear the yearly stats
clearYearlyStats :: ForrestFunction ()
clearYearlyStats = modify (\s -> s{yearHarvests = 0, yearMaulings = 0})

-- Generate random coords that are in bounds
randomCoords :: ForrestFunction Coords
randomCoords = do
				s <- gets size
				r <- gets randGen
				
				let (x, r') = randomR (0, s - 1) r
				let (y, r'') = randomR (0, s - 1) r'
				
				modify (\i -> i{randGen = r''})
				
				return (x, y)

-- Spawn a bear
spawnBear :: ForrestFunction ()
spawnBear = do
				b <- gets bears
				
				newCoords <- randomCoords
				
				modify (\s -> s{bears = (newCoords, 5):b})

-- Trap a bear
trapBear :: ForrestFunction ()
trapBear = do
				b <- gets bears
				
				r <- gets randGen
				
				let (badBear, r') = randomR (0, (length b) - 1) r
				
				let (earlyBears, _:lateBears) = splitAt badBear b
				
				modify (\s -> s{bears = (earlyBears ++ lateBears)})
				
-- Spawn a lumberjack
spawnLumberjack :: ForrestFunction ()
spawnLumberjack = do
					lj <- gets lumberjacks
				
					newCoords <- randomCoords
				
					modify (\s -> s{lumberjacks = (newCoords, 3):lj})

-- Fire a lumberjack
fireLumberjack :: ForrestFunction ()
fireLumberjack = do
					lj <- gets lumberjacks
				
					r <- gets randGen
				
					let (badLumberjack, r') = randomR (0, (length lj) - 1) r
				
					let (earlyLumberjacks, _:lateLumberjacks) = splitAt badLumberjack lj
				
					modify (\s -> s{lumberjacks = (earlyLumberjacks ++ lateLumberjacks)})

-- Restore moves to bears
restoreBearMoves :: ForrestFunction ()
restoreBearMoves = do
					b <- gets bears
					
					let withoutMoves = map fst b
					let resetBears = zip withoutMoves (repeat 5)
					
					modify (\s -> s{bears = resetBears})

-- Restore moves to lumberjacks
restoreLumberjackMoves :: ForrestFunction ()
restoreLumberjackMoves = do
							lj <- gets lumberjacks
							
							let withoutMoves = map fst lj
							let resetLumberjacks = zip withoutMoves (repeat 3)
							
							modify (\s -> s{lumberjacks = resetLumberjacks})

-- 
-- 
-- -- Turn our program into something we can display
-- showProgram :: Program -> String
-- showProgram p = unlines p		
-- 
-- -- Given all the data in a file, we need to turn that into the program and initial stack
-- parseProgram :: String -> (Program, [Int])
-- parseProgram s = (extendedLines ++ extraLines, startingStack)
-- 	where
-- 		(firstLine:_:otherLines)	= lines s												-- Extract the first line		
-- 		extendLine l			= l ++ (replicate (80 - length l) ' ')						-- Function to pad lines to 80 characters
-- 		extendedLines			= map extendLine otherLines
-- 		extraLines				= replicate (25 - length extendedLines) (replicate 80 ' ')	-- Extra lines to make the program 25 rows
-- 		startingStack			= map read $ words firstLine
-- 
-- -- Find the next position given current coords and current direction
-- nextPosition :: Coords -> Direction -> Coords
-- nextPosition (x, y) d = (bigX `mod` 80, bigY `mod` 25)
-- 	where
-- 		(xDelta, yDelta) = case d of
-- 							DLeft	-> (-1, 0)
-- 							DRight	-> (1, 0)
-- 							DUp		-> (0, -1)
-- 							DDown	-> (0, 1)
-- 		(bigX, bigY) = (x + 80 + xDelta, y + 25 + yDelta)		-- Add numbers so we can % without negatives
-- 								
-- -- Get the instruction at the given spot
-- findInstruction :: Program -> Coords -> Instruction
-- findInstruction m (x, y) = m !! y !! x
-- 
-- -- Change an instruction
-- setInstruction :: Program -> Coords -> Instruction -> Program
-- setInstruction p (x, y) i = rowsBefore ++ (lineBefore ++ i:lineAfter):rowsAfter
-- 	where
-- 		(rowsBefore, rowToChange:rowsAfter)	= splitAt y p
-- 		(lineBefore, _:lineAfter)			= splitAt x rowToChange
-- 		
-- -- Get a character from the user
-- askUserForChar :: IO Char
-- askUserForChar = do
-- 					putStr "Enter a character: "
-- 					hFlush stdout
-- 					getChar
-- 
-- -- Get a number from the user
-- askUserForNum :: IO Int
-- askUserForNum = do
-- 					putStr "Enter a number: "
-- 					hFlush stdout
-- 					input <- getLine
-- 					return $ read input
-- 
-- -- Display a character to the user
-- putCharacter :: Int -> IO ()
-- putCharacter i = do
-- 					putChar $ chr i
-- 					hFlush stdout
-- 
-- -- Function to get some stuff from our stack
-- popStack :: Int -> InterpreterFunction [Int]
-- popStack num = state updateFunc
-- 	where
-- 		updateFunc i@(Interpreter st _ _ _ _ _ _)
-- 			| num <= length st	= (taken, i{stack = rest})		-- Stack underflow? Pattern match will fail, bubble up
-- 				where
-- 					(taken, rest) = splitAt num st
-- 
-- -- Function to update the stack
-- pushStack :: [Int] -> InterpreterFunction ()
-- pushStack a = state $ \s -> ((), s{stack = (reverse a) ++ (stack s)})
-- 
-- -- Function to change our upcoming direction
-- setDirection :: Direction -> InterpreterFunction ()
-- setDirection d = state $ \s -> ((), s{direction = d})
-- 
-- -- Update our position
-- updatePosition :: InterpreterFunction ()
-- updatePosition = do
-- 					i@(Interpreter _ po _ di _ _ _) <- get
-- 					put i{position = nextPosition po di}
-- 
-- -- Get a program and initial stack from a file
-- loadProgram :: String -> IO (Program, [Int])
-- loadProgram f = do
-- 			fileText <- readFile f
-- 			return $ parseProgram fileText
-- 
-- -- The function that actually does our computation
-- interpreterLoop :: InterpreterFunction ()
-- interpreterLoop = do
-- 				i@(Interpreter _ po _ di _ _ ru) <- get				-- Get some stuff we need
-- 				
-- 				if not $ running i then
-- 					return ()										-- We're not running, so we're done
-- 				else
-- 					do
-- 						interpretWithStringGuard					-- Run the instruction
-- 						updatePosition								-- Update our position
-- 						interpreterLoop								-- Do it again
-- 
-- -- Handles if we're in string mode or not
-- interpretWithStringGuard :: InterpreterFunction ()
-- interpretWithStringGuard = do
-- 				i@(Interpreter st po pr di str _ _) <- get
-- 
-- 				inst <- return $ findInstruction pr po				-- The instruction that's up next
-- 						
-- 				if inst == '"' then
-- 					put i{stringMode = not str}						-- Toggle string mode
-- 				else if str then
-- 					pushStack [ord inst]							-- Push character value onto stack								
-- 				else
-- 					interpretInstruction inst
-- 
-- -- Run a single instruction knowing we're not in string mode
-- interpretInstruction :: Instruction -> InterpreterFunction ()
-- interpretInstruction inst = do
-- 				i@(Interpreter st po pr di _ rg ru) <- get
-- 								
-- 				case inst of
-- 					'0' -> pushStack [0]
-- 					'1' -> pushStack [1]
-- 					'2' -> pushStack [2]
-- 					'3' -> pushStack [3]
-- 					'4' -> pushStack [4]
-- 					'5' -> pushStack [5]
-- 					'6' -> pushStack [6]
-- 					'7' -> pushStack [7]
-- 					'8' -> pushStack [8]
-- 					'9' -> pushStack [9]
-- 					'+' -> do
-- 							(a:b:_) <- popStack 2
-- 							pushStack [a + b]
-- 					'-' -> do
-- 							(a:b:_) <- popStack 2
-- 							pushStack [b - a]
-- 					'*' -> 	do
-- 							(a:b:_) <- popStack 2
-- 							pushStack [a * b]
-- 					'/' -> do
-- 							(a:b:_) <- popStack 2
-- 							if a == 0
-- 								then fail "Divide by zero"
-- 								else pushStack [b `div` a]	
-- 					'%' -> do
-- 							(a:b:_) <- popStack 2
-- 							if a == 0
-- 								then fail "Modulus by zero"
-- 								else pushStack [b `mod` a]	
-- 					'!' -> do
-- 							[a] <- popStack 1
-- 							if a == 0
-- 								then pushStack [1]
-- 								else pushStack [0]
-- 					'`' -> do
-- 							(a:b:_) <- popStack 2
-- 							if b > a
-- 								then pushStack [1]
-- 								else pushStack [0]
-- 					'>' -> setDirection DRight
-- 					'<' -> setDirection DLeft 
-- 					'^' -> setDirection DUp
-- 					'v' -> setDirection DDown 
-- 					'?' -> do													-- Set a new random direction
-- 							let (newDir, newGen) = random rg
-- 							put i{direction = newDir, randGen = rg}
-- 					'_' -> do
-- 							[a] <- popStack 1
-- 							if a == 0
-- 								then setDirection DRight
-- 								else setDirection DLeft
-- 					'|' -> do
-- 							[a] <- popStack 1
-- 							if a == 0
-- 								then setDirection DDown
-- 								else setDirection DUp
-- 					':' -> do
-- 							[a] <- popStack 1
-- 							pushStack [a, a]
-- 					'\\' -> do
-- 							(a:b:_) <- popStack 2
-- 							pushStack [a, b]
-- 					'$' -> popStack 1 >> return ()								-- Throw away stack value
-- 					'.' -> do													-- Show top stack as integer
-- 							[a] <- popStack 1
-- 							liftIO $ putStr $ show a
-- 					',' -> do													-- Show top stack as character
-- 							[a] <- popStack 1
-- 
-- 							liftIO $ putChar $ chr a
-- 					'#' -> updatePosition										-- Skip a cell by doing an extra move
-- 					'p' ->  do													-- Pop y, x, v then set program at (x,y) to v
-- 							(y:x:v:_) <- popStack 3
-- 							newProg <- return $ setInstruction pr (x, y) (chr v)
-- 							
-- 							updatedState <- get									-- Get the new state with the smaller stack
-- 							
-- 							put updatedState{program = newProg}
-- 					'g' -> do													-- Get the value stored at (x,y)
-- 							(y:x:_) <- popStack 2
-- 
-- 							val <- return $ findInstruction pr (x, y)
-- 
-- 							pushStack [ord val]
-- 					'&' -> do
-- 							num <- liftIO askUserForNum
-- 							pushStack [num]
-- 					'~' -> do
-- 							c <- liftIO askUserForChar
-- 							pushStack [ord c]
-- 					'@' -> put i{running = False} 								-- Mark the program is over
-- 					' ' -> return ()											-- Ignore spaces
-- 
-- -- Runs a Befunge program
-- runProgram :: Interpreter -> IO ()
-- runProgram = evalStateT interpreterLoop

------------------ Our main function, to do the work ------------------

main = do
-- 	args <- getArgs
-- 	
-- 	putStrLn $ "Loading program from " ++ args !! 0
-- 	
-- 	(p, initialStack) <- loadProgram $ args !! 0
	
	putStrLn "Here is the program:"
	
-- 	putStrLn $ showProgram p
-- 	
-- 	randGen <- getStdGen
-- 	
-- 	initialState <- return $ Interpreter initialStack (0, 0) p DRight False randGen True
-- 	
-- 	putStrLn "Starting the interpreter...\n"
-- 
-- 	runProgram initialState
-- 
-- 	putStrLn "\n\nWe're done"
