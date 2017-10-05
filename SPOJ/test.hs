main :: IO () 
main = do 
    all <- getContents 
    let nums = map (read :: String -> Int) $ lines all 
    printIf nums 
    
printIf :: [Int] -> IO () 
printIf [] = return () 
printIf (x:xs) = do 
    if x == 42 then return () 
    else do 
        print x 
        printIf xs

