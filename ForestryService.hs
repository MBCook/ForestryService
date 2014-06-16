-- Simulates a forest
-- Challenge from: http://www.reddit.com/r/dailyprogrammer/comments/27h53e/662014_challenge_165_hard_simulated_ecology_the/

-- I'm getting this state monad thing, let's also make an animated gif!

import System.Environment
import System.Random
import System.IO
import Data.Char
import Text.Printf
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans
import Codec.Picture.Gif
import Codec.Picture.Types
import qualified Data.Map.Lazy as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

------------------ Some types we'll use ------------------

type Tree = Int								-- Age in months, negative is nothing

type Coords = (Int, Int)

type MovesLeft = Int

type Moveables = M.Map Coords MovesLeft		-- Key is position value is moves left

type Lumberjacks = Moveables

type Bears = Moveables

type Forest = V.Vector Tree

type Frame = Image Pixel8

data ForestState = ForestState {
					forest			:: Forest,
					lumberjacks		:: Lumberjacks,
					bears			:: Bears,
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

type ForestFunction a = StateT ForestState IO a

------------------ A bunch of constants to make our life easy ------------------

monthsToRun			= 1500		-- How long our simulation runs

bearMoves			= 5			-- How many squares bears wonder a month
lumberjackMoves		= 3			-- How many squares an LJ can wonder in a month

saplingMax			= 12		-- Maximum age of a sapling
matureMax			= 120

matureTree			= 13		-- Age to spawn mature trees at
noTree				= -1		-- Age of 'no tree'

matureSpawnOfTen	= 1			-- 10% chance of a mature tree spawning
elderSpawnOfTen		= 2			-- 20% chance of elder tree spawning

forestSize			= 100		-- Size of each side of the forrest (total spots is ^2)
treesToHire			= 10		-- Scaling factor determining how many trees an LJ needs to cause a hire
treesToFire			= 5			-- Trees we have to be short to fire someone

lumberjackStarts	= forestSize `div` 10	-- Starting LJs
bearStarts			= forestSize `div` 50	-- Starting bears
treeStarts			= forestSize `div` 2	-- Starting trees

makeGif				= True		-- Should we make an image?

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

isSapling :: Tree -> Bool
isSapling t = t >= 0 && t <= saplingMax

isMature :: Tree -> Bool
isMature t = t > saplingMax && t <= matureMax

isElder :: Tree -> Bool
isElder t = t > matureMax

isClear :: Tree -> Bool
isClear t = t < 0

-- Check if the tree should generate a sapling this month
shouldSpawn :: Tree -> ForestFunction Bool
shouldSpawn t = do
					r <- gets randGen
					
					let (n, r') = randomR (1 :: Int, 10 :: Int) r
					
					modify (\s -> s{randGen = r'})
					
					case () of _
							| isSapling t	-> return False						-- Saplings can't spawn
							| isMature t	-> return $ n == matureSpawnOfTen	-- Mature have 10% chance at spawn each month
							| isElder t		-> return $ n <= elderSpawnOfTen	-- Elders have 20% chance at spawn each month
							| otherwise		-> return False						-- No tree means no spawn
						
-- Given a tree at coords, get the coords it can spawn at. Assume it's allowed to spawn.
possibleSaplingPoints :: Coords -> ForestFunction [Coords]
possibleSaplingPoints c = do
							neighboringCoords <- neighboringCells c				-- Neighboring cells
							neighboringTrees <- mapM getTree neighboringCoords	-- Neighboring trees
							
							let pairs = zip neighboringCoords neighboringTrees	-- Match 'em together so we can filter
							let goodPairs = filter (isClear . snd) pairs		-- Take only empty spots

							return $ map fst goodPairs							-- Return only the coords

-- Spawn a tree at a random location if possible
possiblySpawnTree :: Coords -> ForestFunction ()
possiblySpawnTree c = do
						t <- getTree c
						r <- gets randGen
						sp <- gets sprouts
						
						good <- shouldSpawn t
						
						if not good then											-- Make sure it can spawn
							return ()
						else
							do
								-- We've determined we're spawning, is there a spot to actually do it?
							
								possibleSpots <- possibleSaplingPoints c
							
								if null possibleSpots then							-- Make sure there are spots
									return ()
								else
									do
										-- Yep, pick a spot, make the changes							

										let (n, r') = randomR (0, (length possibleSpots - 1)) r
										
										setTree (possibleSpots !! n) 0				-- A new sapling
										
										modify (\s -> s{sprouts = sp + 1, randGen = r'})

-- Sprout all trees that need it and can
calculateSprouts :: ForestFunction ()
calculateSprouts = do
						s <- gets size
						
						let coords = [(x, y) | x <- [0.. s - 1], y <- [0 .. s- 1]]
						
						mapM_ possiblySpawnTree coords								-- Run each coord through our spawner

-- Find things who need to be moved the given number of spaces
findThingsToMove :: Int -> (ForestState -> Moveables) -> ForestFunction [Coords]
findThingsToMove w f = do
							things <- gets f										-- Use the accessor to get what we need
							
							return $ M.keys $ M.filter ((==) w) things				-- Keep only those with w moves

-- Move a lumberjack, assuming it's OK
moveLumberjack :: Coords -> ForestFunction ()
moveLumberjack c = do
						ljs <- gets lumberjacks
				
						let w = (M.!) ljs  c
				
						possibleWalks <- neighboringCells c
				
						let withoutConflicts = filter (noLJAtCoords ljs) possibleWalks
				
						if null withoutConflicts then										-- They can't move, lose a turn
							do
								let updatedLJs = changeWeight ljs c (w - 1)
					
								modify (\s -> s{lumberjacks = updatedLJs})
						else
							do																-- They CAN move, find the spot
								r <- gets randGen
				
								let (n, r') = randomR (0, (length withoutConflicts - 1)) r
						
								let updatedLJs = changeLocation ljs c (withoutConflicts !! n) (w - 1)
						
								modify (\s -> s{lumberjacks = updatedLJs, randGen = r'})
					where
						noLJAtCoords ljs c			= M.notMember c ljs						-- Check if there is an LJ there
						changeWeight ljs c w		= M.insert c w ljs						-- Update a weight
						changeLocation ljs co cn w	= M.insert cn w $ M.delete co ljs		-- Move an LJ, update weight

-- Move a bear, assuming it's OK
moveBear :: Coords -> ForestFunction ()
moveBear c = do
				bs <- gets bears
			
				let w = (M.!) bs  c
			
				possibleWalks <- neighboringCells c
			
				let withoutConflicts = filter (noBearsAtCoords bs) possibleWalks
			
				if null withoutConflicts then										-- They can't move, lose a turn
					do
						let updatedBears = changeWeight bs c (w - 1)
			
						modify (\s -> s{bears = updatedBears})
				else
					do																-- They CAN move, find the spot
						r <- gets randGen
			
						let (n, r') = randomR (0, (length withoutConflicts - 1)) r
				
						let updatedBears = changeLocation bs c (withoutConflicts !! n) (w - 1)
				
						modify (\s -> s{bears = updatedBears, randGen = r'})
			where
				noBearsAtCoords bs c		= M.notMember c bs						-- Check if there is a bear there
				changeWeight bs c w			= M.insert c w bs						-- Update a weight
				changeLocation bs co cn w	= M.insert cn w $ M.delete co bs		-- Move a bear, update weight

-- Check if the simulation is over (4800 months or no trees left)
simulationOver :: ForestFunction Bool
simulationOver = do
					m <- gets month
					(s, ma, e) <- countTrees
					
					return $ (m >= monthsToRun) || (s + ma + e == 0)

-- Find the index for the given tree in our vector
treeIndex :: Coords -> Int -> Int
treeIndex (x, y) s = y * s + x

-- Get the tree at the given spot
getTree :: Coords -> ForestFunction Tree
getTree c = do
				f <- gets forest
				s <- gets size
			
				let index = treeIndex c s
			
				return $ V.unsafeIndex f index

-- Change a tree
setTree :: Coords -> Tree -> ForestFunction ()
setTree c t = do
				f <- gets forest
				s <- gets size
				
				let index = (treeIndex c s)
				
				let f' = V.modify (\v -> MV.write v index t) f
				
				modify (\s -> s{forest = f'})
					
-- Find the coords of all the neighboring cells
neighboringCells :: Coords -> ForestFunction [Coords]
neighboringCells (x, y) = do
						s <- gets size

						let possibilities = [(x + i, y + j) | i <- [-1..1], j <- [-1..1], (abs i) + (abs j) /= 0]

						return $ filter (\(x, y) -> x >= 0 && y >= 0 && x < s && y < s) possibilities

-- Count the number of trees in each state
countTrees :: ForestFunction (Int, Int, Int)
countTrees = do
				f <- gets forest

				return $ V.foldl' countingFunc (0, 0, 0) f 					-- Use a fold to sum up our trees
			where
				countingFunc (s, m, e) t
									| isSapling t	= (s + 1, m, e)
									| isMature t	= (s, m + 1, e )
									| isElder t		= (s, m, e + 1)
									| otherwise		= (s, m, e)

-- Increment the age of all the trees in the forest
incrementTrees :: ForestFunction ()
incrementTrees = do
					f <- gets forest
					
					let f' = V.map updateTree f
					
					modify (\s -> s{forest = f'})
				where
					updateTree t = if isClear t then t else t + 1

-- Increment the month number
incrementMonth :: ForestFunction ()
incrementMonth = do
					m <- gets month
					
					modify (\s -> s{month = m + 1})

-- Figure out and handle all possible maulings
calculateMaulings :: ForestFunction ()
calculateMaulings = do
						bs <- gets bears
						ljs <- gets lumberjacks
						
						let maulings = M.keys $ M.intersection bs ljs		-- List of spaces with both bears and lumberjacks
						
						mapM_ handleMauling maulings						-- Run each one through our mauling updater

-- Record that a mauling happened
handleMauling :: Coords -> ForestFunction ()
handleMauling c = do
					ljs <- gets lumberjacks
					m <- gets maulings
					ym <- gets yearMaulings
					bs <- gets bears
					
					let updatedLumberjacks = M.delete c ljs					-- Remove the injured lumberjack
					let updatedBears = M.insert c 0 bs						-- Update bear to be done moving this month
					
					modify (\s -> s{maulings = m + 1, yearMaulings = ym + 1,
										lumberjacks = updatedLumberjacks, bears = updatedBears})
					
					if M.null updatedLumberjacks then
						spawnLumberjack					-- Always need at least one LJ
					else
						return ()
		
-- Figure out and handle all possible harvests
calculateHarvests :: ForestFunction ()
calculateHarvests = do
						ljs <- gets lumberjacks
						
						let ljPositions = M.keys ljs
						
						ljsWithTrees <- mapM appendTree ljPositions							-- Append the tree in the LJ's position
						
						let ljsWithTreesAndHarvests = filter availableHarvest ljsWithTrees
						
						let harvests = map fst ljsWithTreesAndHarvests						-- Now that we've filtered, strip tree off
						
						mapM_ handleHarvest harvests			-- Run each one through our harvest updater		
					where
						availableHarvest (_, t) = isMature t || isElder t					-- Figure out if we can harvest here
						appendTree lj			= do										-- Add tree to LJ to make calculations easy
													t <- getTree lj
													return (lj, t)

-- Record that a harvest happened
handleHarvest :: Coords -> ForestFunction ()
handleHarvest c = do
						ljs <- gets lumberjacks
						h <- gets harvests
						yh <- gets yearHarvests
					
						t <- getTree c

						let v = treeValue t
					
						setTree c noTree							-- Remove the tree
						
						let updatedLumberjacks = M.insert c 0 ljs	-- Mark that the LJ's turn is over
					
						modify (\s -> s{harvests = h + v,			-- Update state
										yearHarvests = yh + v,
										lumberjacks = updatedLumberjacks})
					where
						treeValue t = if isElder t then 2 else 1	-- Figure out how much wood is being harvested
						
-- Check if it's a new year
isNewYear :: ForestFunction Bool
isNewYear = do
				m <- gets month
				
				return $ m `mod` 12 == 0

-- Clear the yearly stats
clearYearlyStats :: ForestFunction ()
clearYearlyStats = modify (\s -> s{yearHarvests = 0, yearMaulings = 0})

-- Clear the monthly stats
clearMonthlyStats :: ForestFunction ()
clearMonthlyStats = modify (\s -> s{harvests = 0, sprouts = 0, matures = 0, elderly = 0, maulings = 0})

-- Generate random coords that are in bounds
randomCoords :: ForestFunction Coords
randomCoords = do
				s <- gets size
				r <- gets randGen
				
				let (x, r') = randomR (0, s - 1) r
				let (y, r'') = randomR (0, s - 1) r'
				
				modify (\i -> i{randGen = r''})
				
				return (x, y)

-- Spawn a bear
spawnBear :: ForestFunction ()
spawnBear = do
				bs <- gets bears
				
				newCoords <- randomCoords
				
				let updatedBears = M.insert newCoords bearMoves bs
				
				modify (\s -> s{bears = updatedBears})

-- Trap a bear
trapBear :: ForestFunction ()
trapBear = do
				bs <- gets bears
				
				r <- gets randGen
				
				let (badBear, r') = randomR (0, (M.size bs) - 1) r
				
				let (badCoords, _) = M.elemAt badBear bs
				
				let updatedBears = M.delete badCoords bs

				modify (\s -> s{bears = updatedBears})
				
-- Spawn a lumberjack
spawnLumberjack :: ForestFunction ()
spawnLumberjack = do
					ljs <- gets lumberjacks
				
					newCoords <- randomCoords
				
					let updatedLumberjacks = M.insert newCoords lumberjackMoves ljs
				
					modify (\s -> s{lumberjacks = updatedLumberjacks})

-- Fire a lumberjack
fireLumberjack :: ForestFunction ()
fireLumberjack = do
					ljs <- gets lumberjacks
				
					r <- gets randGen
				
					let (badLumberjack, r') = randomR (0, (M.size ljs) - 1) r
				
					let (badCoords, _) = M.elemAt badLumberjack ljs
				
					let updatedLumberjacks = M.delete badCoords ljs

					modify (\s -> s{lumberjacks = updatedLumberjacks})

-- Restore moves to bears
restoreBearMoves :: ForestFunction ()
restoreBearMoves = do
					bs <- gets bears
					
					let resetBears = M.map (\_ -> bearMoves) bs
					
					modify (\s -> s{bears = resetBears})

-- Restore moves to lumberjacks
restoreLumberjackMoves :: ForestFunction ()
restoreLumberjackMoves = do
							ljs <- gets lumberjacks
							
							let resetLumberjacks = M.map (\_ -> lumberjackMoves) ljs
							
							modify (\s -> s{lumberjacks = resetLumberjacks})
							
-- Draw the forest into an image using our pallet
drawForest :: ForestFunction Frame
drawForest = do
				f <- gets forest
				b <- gets bears
				l <- gets lumberjacks
				s <- gets size
				
				return $ generateImage (scaledColor f s b l) (s * 5) (s * 5)
			where
				oneFifth i = i `div` 5
				scaledColor f s b l x y = colorPixel f s b l (oneFifth x, oneFifth y)

-- Given the current forest, list of bears, lumberjacks, and X/Y coords find the right pixel color
colorPixel :: Forest -> Int -> Bears -> Lumberjacks -> (Int, Int) -> Pixel8
colorPixel f s b l c
				| c `M.member` b	= bearColor
				| c `M.member` l	= lumberjackColor
				| isSapling t		= saplingColor
				| isMature t		= matureColor
				| isElder t			= elderlyColor
				| otherwise			= emptyColor
		where
			t = V.unsafeIndex f index
			index = treeIndex c s					

-- Move the bears who have w moves left
moveBears :: Int -> ForestFunction ()
moveBears w = do
				bears <- findThingsToMove w bears				-- Find the bears who need to move
				
				mapM_ moveBear bears							-- Move them
				
				calculateMaulings								-- Handle any possible maulings

-- Move the lumberjacks who have w moves left
moveLumberjacks :: Int -> ForestFunction ()
moveLumberjacks w = do
						lumberjacks <- findThingsToMove w lumberjacks	-- Find the LJs who need to move
				
						mapM_ moveLumberjack lumberjacks				-- Move them
				
						calculateHarvests								-- Handle any possible harvests

-- Print out the monthly stats
printYearlyStats :: Int -> ForestFunction ()
printYearlyStats hired = do
							mo <- gets month						
				
							let y = mo `div` 12						
				
							h <- gets yearHarvests
							m <- gets yearMaulings
				
							bs <- gets bears
							ljs <- gets lumberjacks
				
							(ns, nm, ne) <- countTrees
										
							liftIO $ printf "Year [%04d]: Forest has %d Trees, %d Saplings, %d Elder Trees, %d Lumberjacks and %d Bears.\n" y nm ns ne (M.size ljs) (M.size bs)
							
							if m > 0 then									
								liftIO $ printf "Year [%04d]: 1 Bear captured by zoo due to %d Maulings.\n" y m
							else
								liftIO $ printf "Year [%04d]: 1 Bear spawned since they had a clean year.\n" y
							
							if hired > 0 then
								liftIO $ printf "Year [%04d]: %d Pieces of lumber harvested %d new Lumberjacks hired.\n" y h hired
							else
								liftIO $ printf "Year [%04d]: %d Pieces of lumber harvested %d Lumberjacks were let go.\n" y h (-1 * hired)
							
-- Print out the yearly stats
printMonthlyStats :: ForestFunction ()
printMonthlyStats = do
						mo <- gets month
						h <- gets harvests
						s <- gets sprouts
						ma <- gets matures
						e <- gets elderly
						mu <- gets maulings
						
						liftIO $ possiblePrintLn "Month" mo h "pieces of lumber harvested by Lumberjacks."
						liftIO $ possiblePrintLn "Month" mo s "new Saplings created."
						liftIO $ possiblePrintLn "Month" mo ma " Saplings became Trees."
						liftIO $ possiblePrintLn "Month" mo e " Trees became Elder Trees."
						liftIO $ possiblePrintLn "Month" mo mu " Lumberjacks were Maw'd by a bear."
						
-- Handle the basic formatting we do all the time, and only print if X is non-zero
possiblePrintLn :: String -> Int -> Int -> String -> IO ()
possiblePrintLn t m x s = do
							if x <= 0 then
								return ()
							else
								printf "%s [%04d]: [%d] %s\n" t m x s

-- Function that, once a year, does our yearly stuff
possibleYearlyUpdate :: ForestFunction ()
possibleYearlyUpdate = do
							newYear <- isNewYear
							
							if not newYear then
								return ()
							else
								do
									-- We need to figure out if we need to trap a bear, spawn a bear, and/or hire LJs
									
									m <- gets yearMaulings
									
									if m == 0 then
										spawnBear
									else
										trapBear
									
									-- Figure out how many LJs we need to add / remove
									
									h <- gets yearHarvests
									ljs <- gets lumberjacks

									let needed = lumberjacksNeeded h (M.size ljs)
									
									if needed > 0 then
										sequence_ (replicate needed spawnLumberjack)
									else
										sequence_ (replicate (min (-1 * needed) (M.size ljs)) fireLumberjack)
										
									newLjs <- gets lumberjacks
									
									if M.null newLjs then
										spawnLumberjack					-- Never run out of lumberjacks
									else
										return ()						-- We still have LJs, nothing special to do
									
									printYearlyStats needed				-- Print the year's events
									
									clearYearlyStats					-- Clear the stats

-- Figure out how many lumberjacks are needed (or extra)
lumberjacksNeeded :: Int -> Int -> Int
lumberjacksNeeded h l
					| h < l		= extra `div` treesToFire - 1
					| otherwise	= extra `div` treesToHire + 1 
				where
					extra = h - l

-- Do one month's worth of work
monthlyUpdate :: ForestFunction ()			
monthlyUpdate = do					
					clearMonthlyStats							-- Reset our counters
					
					restoreBearMoves							-- Set both to be able to move again
					restoreLumberjackMoves
					
					(oldSap, oldMat, oldEld) <- countTrees		-- Figure out the 'old' counts
					
					incrementTrees								-- Make every tree older
					
					calculateSprouts							-- Make new trees
					
					(newSap, newMat, newEld) <- countTrees		-- Re-count trees so we can update stats
					
					modify (\s -> s{sprouts = newSap - oldSap,	-- Update tree counts
									matures = newMat - oldMat,
									elderly = newEld - oldEld})

					mapM_ moveLumberjacks [3,2,1]				-- Move lumberjacks 3 times					
					mapM_ moveBears [5,4..1]					-- Move bears 5 times
					
					if makeGif then
						do
							frame <- drawForest
					
							modify (\s -> s{frames = frame : (frames s)})	-- Draw a new GIF frame
					else
						return ()
					
					printMonthlyStats							-- Print this month's stats
					
					incrementMonth								-- Increment the month

					possibleYearlyUpdate						-- Possibly handle our yearly stuff
					
					done <- simulationOver
					
					if done then
						return ()								-- If we're done don't go any further
					else
						monthlyUpdate							-- We've got more to do, do it again!					
						
-- Generates an initial state for us
initializeForest :: ForestFunction ()
initializeForest  = do
						s <- gets size
						
						let blankForest = V.replicate (s * s) noTree		-- Empty forest
						
						lumberjackCoords <- replicateM lumberjackStarts randomCoords
						bearCoords <- replicateM bearStarts randomCoords
						
						let ljs = zip lumberjackCoords (repeat 0)
						let bs = zip bearCoords (repeat 0)
						
						modify (\s -> s{forest = blankForest, bears = M.fromList bs, lumberjacks = M.fromList ljs})
						
						-- Now we'll fill our forest with trees
						
						treeCoords <- replicateM treeStarts randomCoords
						
						mapM (flip setTree matureTree) treeCoords	-- Start with mature trees
						
						-- And setup the initial frame
						
						if makeGif then
							do						
								frame <- drawForest
					
								modify (\s -> s{frames = [frame]})	
						else
							return ()

-- Finishes initialization and runs everything
runSimulation :: ForestFunction ()
runSimulation = do
					initializeForest
					monthlyUpdate

------------------ Our main function, to do the work ------------------

main = do
	randGen <- getStdGen

	let initialState = ForestState V.empty M.empty M.empty 0 0 0 0 0 0 0 0 randGen [] forestSize
	
	putStrLn "Simulating the forest...\n"
	
	result <- execStateT runSimulation initialState
	
	-- We have to write out our result image

	putStrLn "\nDone"
	
	if makeGif then
		do
			putStrLn "\nWriting out the image."
	
			let fin = writeGifImages "output.gif" LoopingNever $ map (\x -> (ourPallet, 10, x)) (reverse $ frames result)
	
			either  (\a -> putStrLn $ "Error saving gif: " ++ a) id fin 
	else
		return ()
