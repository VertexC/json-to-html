import System.Environment
import Control.Monad
import System.IO

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
scanner (x:xs) content | (x == '{') 
                       | other wise = scanner xs content


main = do 
    [f, g] <- getArgs
    s <- readFile f
    let tokens = scanner s ""
    
