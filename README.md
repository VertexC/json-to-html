```bash
ghc -o hello hello.hs
$ ./hello
Hello, World!
```


don't mess <- and let in do stmt


```haskell
<!-- haskell class -->
```


```haskell
<!-- when statement! -->
import Control.Monad
main = do s <- getLine
          when (s == "foo") $ putStr "You entered foo"
```

[About $ and .](https://stackoverflow.com/questions/940382/what-is-the-difference-between-dot-and-dollar-sign)


语法糖（Syntactic sugar），也译为糖衣语法，是由英国计算机科学家彼得·蘭丁发明的一个术语，指计算机语言中添加的某种语法，这种语法对语言的功能没有影响，但是更方便程序员使用。 语法糖让程序更加简洁，有更高的可读性


[Maybe and Just](https://stackoverflow.com/questions/18808258/what-does-the-just-syntax-mean-in-haskell)


### regular expression
https://stackoverflow.com/questions/8508919/haskell-regex-syntax
The problem is that you simply don't have the regex-posix package installed. This is the package that exports the Text.Regex.Posix module.  Text.Read is exported by the base package which comes with every Haskell distribution.

You can see this by running ghc-pkg find-module Text.Read. To install the regex-posix package globally run the command cabal install regex-posix. If you don't want to install it globally or run into problems getting it to install, it would be better to try installing it with the same command in a sandbox after running cabal sandbox init in the directory of your choice.

#### pcre
[cheatsheet](https://www.debuggex.com/cheatsheet/regex/pcre)\
```haskell
let stringResult = "hello there" =~ "e" :: AllTextMatches [] String
let result = getAllTextMatches stringResult
```
**issue**
install pcre 
```bash
Wrap.hsc:148:10: fatal error: 'pcre.h' file not found
#include <pcre.h>
```
**solution**
https://github.com/facebook/duckling/pull/6

cabal install regex-posix
### cabal
to use ghci with packages in local sandboxs
```bash
# For GHC < 7.6
$ ghci -no-user-package-conf -package-conf .cabal-sandbox/i386-linux-ghc-7.4.2-packages.conf.d
```
to make hs file using sandboxs
```bash
cabal exec -- ghc --make diagramstutorial.lhs
```

ghci
:quit 


### procedure
muti level if guard
```haskell
parseNumber :: String -> Maybe (String, String)
parseNumber [] = Just ("", "")
parseNumber (h:ls)
    | isDigit h =
         case () of
           () | p == Nothing = Just([h], ls)
              | otherwise = Just (h:fst d, snd d) -- Ends in a digit
    | h == '.' =
         case () of
           () | p == Nothing = Nothing
              | not ('.' `elem` (snd d)) = Just (h:(fst d), snd d)
    | otherwise = Nothing
    where 
        p      = parseNumber ls
        Just d = parseNumber ls
```

**warning**
```haskell
show '"'
"'\"'":: String
```
conver char to string
'''haskell
str_a = ['a']
'''

.e. [1,2,3]!!1 gives you 2, since lists are 0-indexed.

# if in do
https://stackoverflow.com/questions/5920888/haskell-how-can-i-use-if-statement-in-do-block-properly

action = do
    isdir <- doesDirectoryExist path
    if not isdir
        then handleWrong
        else return ()
    doOtherActions
