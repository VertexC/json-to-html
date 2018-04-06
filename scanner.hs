import System.Environment
import Control.Monad
import System.IO
import Text.Regex.PCRE

-- BRACE          // {}
-- REGEX          // []
-- COMMA          // ,
-- COLON          // :
-- STRING         // "...."
-- NUMBER
-- BOOLEAN

data Type = BRACE | REGEX | COMMA | COLON | STRING | NUMBER | BOOLEAN deriving Show
type Token = (String, Type)


-- scan and tokenize
scanner :: String -> String -> Bool -> Bool -> [Token]
scanner [] _ _ _ = []
scanner (x:xs) content str_flag slash_flag
                      --  start of string
                       | (str_flag == False && x == '"') = scanner xs "" True False
                      --  read in " but after \
                       | (str_flag == True && x == '"' && slash_flag == True) = scanner xs (content++[x]) True False
                      --  reand in ", end of string
                       | (str_flag == True && x == '"' && slash_flag == False) = (content, STRING):(scanner xs "" False False)                  
                      --  read in a char
                       | (str_flag == True && x /= '"' && x /= '\\') = scanner xs (content++[x]) True False
                      --  read in \
                       | (str_flag == True && x == '\\') = scanner xs (content++("\\")) True True
                      --  | (x == '[') = (show x, REGEX):(scanner xs content str_flag slash_flag)
                      --  | (x == '{') = (show x, BRACE):(scanner xs content str_flag slash_flag)
                       | otherwise = scanner xs content str_flag slash_flag

-- show the Token
printToken :: [(String, Type)] -> IO ()
printToken [] = return ()
printToken ((content, t):xs) = do 
  appendFile "output.html" ((show content) ++ "<=" ++ (show t) ++ "\n")    
  putStrLn(content)
  printToken xs

-- show the ch of String
printString :: String -> IO String
printString [] = return ""
printString (x:xs) = do
  appendFile "output.html" (show x ++ "\n")  
  printString xs


main = do 
    [f, g] <- getArgs
    s <- readFile f
    -- clean up the file
    writeFile "output.html" ("")
    -- printString(s)
    let tokens = scanner s "" False False
    printToken(tokens)
