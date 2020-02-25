module Main
    (
    main
    ) where

import Glob
import System.Environment(getArgs)

main :: IO ()
main =  getArgs >>= namesMatchings >>= prints

prints :: [[String]] -> IO()
prints xs =  mapM_ print $ concat xs
