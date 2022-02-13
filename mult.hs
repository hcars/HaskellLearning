import System.Environment   
import Data.List 

mult :: Int -> Int -> Int
mult x y
 |(x < 0) && (y < 0) = mult (-x) (-y)
 |(x < 0) || (y < 0) = - (mult ( abs x) (abs y ))
 |y == 0 = 0
 | y > 0 = x + (mult x (y-1))


main =	do
 	args <- getArgs                  -- IO [String]
        progName <- getProgName          -- IO String
        putStrLn "The arguments are:"  
        mapM putStrLn args  
        putStrLn "The program name is:"  
    	putStrLn progName
        putStrLn "The result is:"
        let result = mult (read (args !! 0)) (read (args !! 1))
	print result
