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
scanner :: String -> String -> [Token]
scanner [] _ = []
scanner (x:xs) content | (x == '[') = (show x, REGEX):(scanner xs content)
                       | (x == '{') = (show x, BRACE):(scanner xs content)
                       | otherwise = scanner xs content

-- show the Token
printToken :: [(String, Type)] -> IO ()
printToken [] = return ()
printToken ((content, t):xs) = do 
  putStrLn(content)
  putStrLn(show t)
  printToken xs

main = do 
    [f, g] <- getArgs
    s <- readFile f
    let tokens = scanner s ""
    printToken(tokens)
