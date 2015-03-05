import Control.Concurrent
import Control.Exception
import System.Directory
import System.Environment
import System.Exit
import System.IO

import qualified Grid as Grid

clearScreen :: IO ()
clearScreen = putStrLn $ take 50 $ repeat '\n'

mainLoop :: Grid.Grid -> IO ()
mainLoop g = do
    clearScreen
    let ng = Grid.nextGridState g
    putStrLn $ show ng
    threadDelay 500000
    mainLoop ng

runPulsar :: IO ()
runPulsar = do
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
    mainLoop $ Grid.initialize emptyGrid startingCells

processFile :: Handle -> IO Grid.Grid
processFile f = do
    cStr <- hGetLine f
    rStr <- hGetLine f
    sGridStr <- hGetContents f
    let c = read cStr :: Int
    let r = read rStr :: Int
    let sGrid = read sGridStr :: [(Int, Int)]
    let g = Grid.initialize (Grid.create c r) sGrid
    return (g)

main :: IO()
main = do
    args <- getArgs
    case args of
        [] -> runPulsar
        (a:_) -> do
            fileExists <- doesFileExist a
            if fileExists
                then do
                    f <- openFile a ReadMode
                    g <- processFile f
                    mainLoop g
                    hClose f
                    exitSuccess
                else do
                    hPutStrLn stderr $ "Error: File \"" ++ a ++ "\"does not exist."
                    exitFailure


