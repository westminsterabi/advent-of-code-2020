import System.IO

finalSum :: Int
finalSum = 2020

makeInteger :: [String] -> [Int]
makeInteger = map read 

checkIndices :: [Int] -> Int -> Int -> Int -> Bool
checkIndices nums sum x y = 
    nums!!x + nums!!y == sum

findSumsAndMultiply :: [Int] -> Int -> Int -> Int -> Maybe Int
findSumsAndMultiply nums sum i j 
    | i == len - 1 = Nothing 
    | j == len = findSumsAndMultiply nums sum (i+1) (i+2)
    | checkIndices nums sum i j = Just (nums!!i * nums!!j)
    | otherwise = findSumsAndMultiply nums sum i (j+1)
    where
        len = length nums

main = do 
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let l = lines contents 
        intList = makeInteger l 
    putStrLn . show $ findSumsAndMultiply intList finalSum 0 0 
    hClose handle 