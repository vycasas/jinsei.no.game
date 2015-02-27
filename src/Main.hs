import Control.Concurrent
import Grid

clearScreen :: IO ()
clearScreen = putStrLn $ take 50 $ repeat '\n'

mainLoop :: Grid -> IO ()
mainLoop g = do
    clearScreen
    let ng = nextGridState g
    putStrLn $ show ng
    threadDelay 500000
    mainLoop ng

main :: IO()
main = do
    -- Pulsar
    let emptyGrid = Grid.create 15 15
    let startingCells = [
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
    mainLoop $ initialize emptyGrid startingCells
