module Grid where

newtype Grid = Grid [[Bool]]

instance Show Grid where
    show (Grid g) = (foldl (\acc i -> acc ++ i ++ "\n") "" createLineStr)
        where
            --createTopLayer = foldl (\acc _ -> acc ++ "_ ") " " $ g !! 0
            --createLineStr = fmap (foldl (\acc i -> if i then acc ++ "*|" else acc ++ "_|") "|") g
            createLineStr = fmap (foldl (\acc i -> if i then acc ++ "■|" else acc ++ "□|") "|") g

showAsList :: Grid -> String
showAsList (Grid g) = show g

create :: Int -> Int -> Grid
create c r = Grid $ take r $ repeat $ take c $ repeat False

size :: Grid -> (Int, Int)
size (Grid g) = (length $ g !! 0, length g)

isCellAlive :: Grid -> Int -> Int -> Bool
isCellAlive (Grid g) c r = (g !! (r - 1)) !! (c - 1)

setCellLife :: Grid -> Int -> Int -> Bool -> Grid
setCellLife (Grid g) c r f = Grid $ firstPart ++ updatedPart ++ lastPart
    where
        firstPart = take (r - 1) g
        part = (g !! (r - 1))
        updatedPart = [(take (c - 1) part) ++ [f] ++ (drop c part)]
        lastPart = drop r g

createCell :: Grid -> Int -> Int -> Grid
createCell g c r = setCellLife g c r True

killCell :: Grid -> Int -> Int -> Grid
killCell g c r = setCellLife g c r False

initialize :: Grid -> [(Int, Int)] -> Grid
initialize g [] = g
initialize g ((c, r):xs) = initialize updatedBoard xs
    where
        updatedBoard = createCell g c r

neighbours :: Grid -> Int -> Int -> [(Int, Int)]
neighbours g c r = n1 ++ n2 ++ n3 ++ n4 ++ n5 ++ n6 ++ n7 ++ n8
    where
        (cLimit, rLimit) = size g
        -- n1 = (c - 1), (r - 1)
        n1 = if (c - 1) < 1 || (r - 1) < 1 then [] else [((c - 1), (r - 1))]
        -- n2 = c, (r - 1)
        n2 = if (r - 1) < 1 then [] else [(c, (r - 1))]
        -- n3 = (c + 1), (r - 1)
        n3 = if (c + 1) > cLimit || (r - 1) < 1 then [] else [((c + 1), (r - 1))]
        -- n4 = (c - 1), r
        n4 = if (c - 1) < 1 then [] else [((c - 1), r)]
        -- n5 = (c + 1), r
        n5 = if (c + 1) > cLimit then [] else [((c + 1), r)]
        -- n6 = (c - 1), (r + 1)
        n6 = if (c - 1) < 1 || (r + 1) > rLimit then [] else [((c - 1), (r + 1))]
        -- n7 = c, (r + 1)
        n7 = if (r + 1) > rLimit then [] else [(c, (r + 1))]
        -- n8 = (c + 1), (r + 1)
        n8 = if (c + 1) > cLimit || (r + 1) > rLimit then [] else [((c + 1), (r + 1))]

neighbourStates :: Grid -> Int -> Int -> [Bool]
neighbourStates g c r = map (\(cx, rx) -> isCellAlive g cx rx) $ neighbours g c r

countAliveNeighbours :: Grid -> Int -> Int -> Int
countAliveNeighbours g c r = length $ filter (\i -> i) $ neighbourStates g c r

countDeadNeighbours :: Grid -> Int -> Int -> Int
countDeadNeighbours g c r = length $ filter (\i -> not i) $ neighbourStates g c r

nextGridState :: Grid -> Grid
nextGridState g = Grid $ map processRow [1 .. rLimit]
    where
        (cLimit, rLimit) = size g
        processRow r = map (processCol r) [1 .. cLimit]
        processCol r c =
            if isCellAlive g c r
                then (liveNeighborCount > 1) && (liveNeighborCount < 4)
                else liveNeighborCount == 3
                    where
                        liveNeighborCount = countAliveNeighbours g c r
{-
-- test
xGrid :: Grid
xGrid = initialize emptyGrid aliveCells
    where
        emptyGrid = Grid.create 15 15
        aliveCells = [
            (4, 2), (5, 2), (6, 2),
            (10, 2), (11, 2), (12, 2),
            (2, 4), (2, 5), (2, 6),
            (7, 4), (7, 5), (7, 6),
            (9, 4), (9, 5), (9, 6),
            (14, 4), (14, 5), (14, 6),
            (4, 7), (5, 7), (6, 7),
            (10, 7), (11, 7), (12, 7),
            (4, 9), (5, 9), (6, 9),
            (10, 9), (11, 9), (12, 9),
            (2, 10), (2, 11), (2, 12),
            (7, 10), (7, 11), (7, 12),
            (9, 10), (9, 11), (9, 12),
            (14, 10), (14, 11), (14, 12),
            (4, 14), (5, 14), (6, 14),
            (10, 14), (11, 14), (12, 14)
            ]
-}
