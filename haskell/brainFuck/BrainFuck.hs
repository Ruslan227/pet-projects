module BrainFuck 
     where

-- import Control.Monad.Reader    
import Control.Monad.State
import Data.Char(chr, ord) 

main :: IO ()
main = readAllLinesAndRun []

readAllLinesAndRun :: String -> IO ()
readAllLinesAndRun program = do 
    s <- getLine
    if null s
    then run ([], replicate 30000 (chr 0)) ([], program)
    else readAllLinesAndRun $ program ++ s

data Command = 
    Write            |
    Read             |
    Continue         | 
    MoveToCycleEnd   |
    MoveToCycleStart |
    Error String

type Program = (String, String)
type CompilerCharList = (String, String)
type Configuration = State CompilerCharList Command

run :: CompilerCharList -> Program -> IO ()
run conf (hpr, []) = return ()
run conf (hpr, pr) = do
        let (res, st) = runState (runCommand $ head pr) conf
        case (res, st) of
            (Continue, st)         -> run st pr'
            (Write, st)            -> writeChar st
            (Read, st)             -> readChar st
            (Error m, st)          -> error m  
            (MoveToCycleEnd, st)   -> run st (moveToCycleEnd pr')
            (MoveToCycleStart, st) -> run st (moveToCycleStart $ moveLeftToRight hpr pr)
    where
        pr' = moveRightToLeft hpr pr 
        readChar, writeChar :: CompilerCharList -> IO ()
        readChar (h, f:t) = do
            c <- getChar
            run (h, c:t) pr'
        writeChar (h, t) = do     
            print $ head t
            run (h, t) pr'

moveRightToLeft, moveLeftToRight :: String -> String -> Program
moveRightToLeft _ [] = error "empty right list"
moveRightToLeft arr1 arr2 = (arr1 ++ [head arr2], tail arr2)

moveLeftToRight [] _ = error "empty left list"
moveLeftToRight arr1 arr2 = let (l:arr1') = reverse arr1 in (reverse arr1', l:arr2)

moveToCycleEnd, moveToCycleStart :: Program -> Program    
moveToCycleEnd = moveToCycleEnd' 1
    where
    moveToCycleEnd' :: Int -> Program -> Program
    moveToCycleEnd' bal (hpr, r@(x:pr)) = case x of 
            ']' -> if bal == 1
                   then moveRightToLeft hpr r
                   else goNext (pred bal) hpr r
            '[' -> goNext (succ bal) hpr r
            _   -> goNext bal hpr r 
        where goNext bal hpr r = moveToCycleEnd' bal (moveRightToLeft hpr r)

moveToCycleStart = moveToCycleStart' (-1)
    where
    moveToCycleStart' :: Int -> Program -> Program
    moveToCycleStart' bal p@(hpr, r@(x:pr)) = case x of 
            '[' -> if bal == -1
                   then p 
                   else goNext (succ bal) hpr r
            ']' -> goNext (pred bal) hpr r
            _   -> goNext bal hpr r
        where goNext bal hpr r = moveToCycleStart' bal (moveLeftToRight hpr r)


runCommand :: Char -> Configuration
runCommand x = do
    case x of
        '>' -> next
        '<' -> previous
        '+' -> inc
        '-' -> dec
        '.' -> return Write
        ',' -> return Read    
        '[' -> cycleStart
        ']' -> cycleEnd
        _ -> return $ Error $ "incorrect symbol: " ++ [x]

commandBuilder :: (CompilerCharList -> Configuration) -> Configuration
commandBuilder f = do
    compilerList <- get 
    f compilerList

next, previous, inc, dec, cycleStart, cycleEnd :: Configuration
next', previous', inc', dec', cycleStart', cycleEnd' :: CompilerCharList -> Configuration

next = commandBuilder next'
next' (h, t) = putAndReturnConfiguration (moveRightToLeft h t) Continue

previous = commandBuilder previous'
previous' (h, t) = putAndReturnConfiguration (moveLeftToRight h t) Continue

inc = commandBuilder inc'
inc' (h, t) = do 
    let incEl = chr $ ord (head t) + 1
    putAndReturnConfiguration (h, incEl : tail t) Continue
               
dec = commandBuilder dec'
dec' (h, t) = do    
    let incEl = chr $ ord (head t) - 1
    putAndReturnConfiguration (h, incEl : tail t) Continue

cycleStart = commandBuilder cycleStart' 
cycleStart' (h, t) = ifElseConfiguration (ord (head t) == 0) MoveToCycleEnd Continue

cycleEnd = commandBuilder cycleEnd' 
cycleEnd' (h, t) = ifElseConfiguration (ord (head t) == 0) Continue MoveToCycleStart

ifElseConfiguration :: Bool -> Command -> Command -> Configuration
ifElseConfiguration isTrue c1 c2 = if isTrue then return c1 else return c2

putAndReturnConfiguration :: CompilerCharList -> Command -> Configuration     
putAndReturnConfiguration list command = do
    put list
    return command