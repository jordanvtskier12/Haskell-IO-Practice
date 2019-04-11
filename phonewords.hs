{-
    This program will build on dictionary.hs and wordsToPhone from a previous
    assignment. You can copy your wordsToPhone source code here or you can simply
    include the line:
    
    import PTfuncsyntax
    
    and run this program in the same directory with your PFfuncsyntax.hs file.
    
    This program will ask the user to enter a 4-digit number. It will then list 
    off all of the english words that can be formed from that number on a standard 
    telephone keypad.
    
    Example of use:
    
    *Main> main
    Type a four-digit number:
    2376
    "Afro"
    "Bern"
    "berm"
    *Main> 

-}

wordsToPhone :: String -> Int

wordsToPhone w = numListToNum (fakewordsToPhone w) 
  

fakewordsToPhone :: String -> [Int]
fakewordsToPhone w 
  |w == [] = []
  |charToPhoneDigit (head w) == 0 = fakewordsToPhone (tail w)
  |otherwise = [charToPhoneDigit (head w)] ++ fakewordsToPhone (tail w)
  

    
  
    
--worddds = filter (`wtf` num) (dictionary)



main = do
    putStrLn "Type a four-digit number:"
    num <- readLn
    dict <- readFile "/usr/share/dict/american-english"
    let dictionary = words dict
    let worddds = filter (`wtf` num) (dictionary)
    plswork worddds
    
    
    
    
charToPhoneDigit :: Char -> Int
charToPhoneDigit c 
    | c == 'a' = 2
    | c == 'b' = 2
    | c == 'c' = 2
    | c == 'A' = 2
    | c == 'B' = 2
    | c == 'C' = 2
    | c == 'd' = 3
    | c == 'D' = 3
    | c == 'e' = 3
    | c == 'E' = 3
    | c == 'f' = 3
    | c == 'F' = 3
    | c == 'g' = 4
    | c == 'G' = 4
    | c == 'h' = 4
    | c == 'H' = 4
    | c == 'i' = 4
    | c == 'I' = 4
    | c == 'j' = 5
    | c == 'J' = 5
    | c == 'k' = 5
    | c == 'K' = 5
    | c == 'l' = 5
    | c == 'L' = 5
    | c == 'M' = 6
    | c == 'm' = 6
    | c == 'n' = 6
    | c == 'N' = 6
    | c == 'o' = 6
    | c == 'O' = 6
    | c == 'p' = 7
    | c == 'P' = 7
    | c == 'q' = 7
    | c == 'Q' = 7
    | c == 'r' = 7
    | c == 'R' = 7
    | c == 's' = 7
    | c == 'S' = 7
    | c == 't' = 8
    | c == 'T' = 8
    | c == 'u' = 8
    | c == 'U' = 8
    | c == 'v' = 8
    | c == 'V' = 8
    | c == 'w' = 9
    | c == 'W' = 9
    | c == 'x' = 9
    | c == 'X' = 9
    | c == 'y' = 9
    | c == 'Y' = 9
    | c == 'z' = 9
    | c == 'Z' = 9
    | otherwise = 0


numList :: [Int] -> String
numList [] = []
numList n = (show (head n) ++ numList (tail n))


numListToNum :: [Int] ->  Int
numListToNum n = read (numList n)::Int

plswork [] = putStrLn ""
plswork x = do 
    putStrLn (show (head x))
    plswork (tail x)
    
wtf x n = wordsToPhone x == n