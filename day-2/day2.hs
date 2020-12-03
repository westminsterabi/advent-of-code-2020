import           Data.List.Split
import           System.IO

data PasswordRule = PasswordRule
  { letter     :: Char
  , upperLimit :: Int
  , lowerLimit :: Int
  }

stringToRule :: String -> PasswordRule
stringToRule str =
  PasswordRule
    { letter = letter
    , upperLimit = read upper :: Int
    , lowerLimit = read lower :: Int
    }
  where
    splitLimits = splitOn "-" str
    lower = head splitLimits
    upper = head $ splitOn " " $ head (tail splitLimits) 
    letter = last $ head (splitOn ":" str)
    password = last (splitOn " " str)

genObeysRule :: String -> Bool 
genObeysRule inputStr =
    obeysRule' pw rule 
    where 
        pw = last (splitOn " " inputStr)
        rule = stringToRule inputStr

obeysRule :: Int -> String -> PasswordRule -> Bool
obeysRule occurences pw rule
  | null pw = occurences <= (upperLimit rule) && occurences >= (lowerLimit rule)
  | (head pw) == (letter rule) = obeysRule (occurences + 1) (tail pw) rule
  | otherwise = obeysRule occurences (tail pw) rule

obeysRule' :: String -> PasswordRule -> Bool 
obeysRule' pw rule = 
    (checkLetterIndex (upperLimit rule) /= (checkLetterIndex (lowerLimit rule)))
    where 
        checkLetterIndex = \i -> pw!!(i-1) == (letter rule)

main = do 
    handle <- openFile "input.txt" ReadMode 
    contents <- hGetContents handle 
    putStrLn (show (length $ filter genObeysRule (lines contents)))
    