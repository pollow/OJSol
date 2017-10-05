import Control.Monad
isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x =
  let upperBound = ceiling $ sqrt $ fromIntegral x
      isPrimeHelper i
        | i > upperBound = True
        | mod x i == 0 = False
        | otherwise = isPrimeHelper $ i + 1
  in isPrimeHelper 2

main :: IO ()
main = do
  line <- getLine
  let times = read line
  generatePrimes times

generatePrimes :: Int -> IO ()
generatePrimes 0 = return ()
generatePrimes i = do
  line <- getLine
  let (l : [r]) = map (read :: String -> Int) $ words line
  printAllPrime l r
  when (i /= 1) $ putStrLn ""
  generatePrimes $ i-1

printAllPrime l r
  | l > r = return ()
  | otherwise = do
      when (isPrime l) (print l)
      printAllPrime (l+1) r
