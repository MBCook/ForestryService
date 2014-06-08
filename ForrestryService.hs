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
							
-- Draw the forrest into an image using our pallet
drawForrest :: ForrestFunction Frame
drawForrest = do
				f <- gets forrest
				b <- gets bears
				l <- gets lumberjacks
				s <- gets size
				
				return $ generateImage (colorPixel f b l) s s

-- Given the current forrest, list of bears, lumberjacks, and X/Y coords find the right pixel color
colorPixel :: Forrest -> [Bear] -> [Lumberjack] -> Int -> Int -> Pixel8
colorPixel f b l x y
		| isJust $ c `lookup` b	= bearColor
		| isJust $ c `lookup` l	= lumberjackColor
		| isSapling t			= saplingColor
		| isMature t			= matureColor
		| isElder t				= elderlyColor
		| otherwise				= emptyColor
	where
		c = (x, y)
		t = f !! y !! x					

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
