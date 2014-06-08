-- Simulates a forrest
-- Challenge from: http://www.reddit.com/r/dailyprogrammer/comments/27h53e/662014_challenge_165_hard_simulated_ecology_the/

-- I'm getting this state monad thing, let's also make an animated gif!

import System.Environment
import System.Random
import System.IO
import Data.Char
import Data.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans
import Codec.Picture.Gif
import Codec.Picture.Types

------------------ Some types we'll use ------------------

type Tree = Int				-- Age in months

type PossibleTree = Maybe Tree

type Coords = (Int, Int)

type Lumberjack = Coords

type Bear = Coords

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
					width			:: Int,
					height			:: Int
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
isMature Nothing		= False
isMature t				= not $ (isSapling t && isElder t)

isElder :: PossibleTree -> Bool
isElder (Just t)	= t > 120
isElder _			= False

-- Check if the tree should generate a sapling this month
-- shouldSpawn :: (Maybe Tree, StdGen) -> (Bool, StdGen)
-- shouldSpawn (Nothing, s) = (Fasle, s)
-- shouldSpawn (Just t, s)	= case t of
-- 							isSapling t	-> (False, s)
-- 							isElder t	-> (num <= 0.2, s')
-- 											where
-- 												(num, s') = randomR (0.0, 1.0) s
-- 							_			-> (num <= 0.1, s')
-- 											where
-- 												(num, s') = randomR (0.0, 1.0) s

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
						w <- gets width
						h <- gets height
						
						let possibilities = [(x + i, y + j) | i <- [-1..1], j <- [-1..1], i /= 0 && j /= 0]

						return $ filter (\(x, y) -> x >= 0 && y >= 0 && x < w && y < h) possibilities

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
									| otherwise		= (s, m, e)

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
