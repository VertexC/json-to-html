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

data Type = BRACE | REGEX | COMMA | COLON | STRING | NUMBER | BOOLEAN | EMPTY | NOTEXIST deriving (Show, Eq)
type Token = (String, Type)



numOrBool :: String -> Type
numOrBool [] = EMPTY
numOrBool content 
  -- match number
  | length(getAllTextMatches (content =~ "-?\\d+(\\.\\d+)?([eE][+-]?\\d+)?" :: AllTextMatches [] String)) == 1 &&
      (content == (getAllTextMatches (content =~ "-?\\d+(\\.\\d+)?([eE][+-]?\\d+)?" :: AllTextMatches [] String))!!0)
        = NUMBER
  -- match boolean
  | content == "true" || content == "false" = BOOLEAN
  | otherwise = NOTEXIST
-- mathc 

-- scan and tokenize
scanner :: String -> String -> Bool -> Bool -> [Token]
scanner [] _ _ _ = []
scanner (x:xs) content str_flag slash_flag
-- string part
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
-- string part end
-- symbol part
  | (x == '{' || x == '}') = (content, numOrBool content):([x], BRACE):(scanner xs "" False False)
  | (x == '[' || x == ']') = (content, numOrBool content):([x], REGEX):(scanner xs "" False False)
  | (x == ',') = (content, numOrBool content):([x], COMMA):(scanner xs "" False False)
  | (x == ':') = (content, numOrBool content):([x], COLON):(scanner xs "" False False)
 -- symbol part end  
  | (x == '\n' || x == ' ') = (content, numOrBool content):(scanner xs "" False False)
  | otherwise = scanner xs (content++[x]) False False

-- show the Token
printToken :: [(String, Type)] -> IO ()
printToken [] = return ()
printToken ((content, t):xs) = do 
  if(t == EMPTY)
    then do 
      printToken xs
    else  
      do appendFile "output.html" ((show content) ++ "<=" ++ (show t) ++ "\n")    
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
