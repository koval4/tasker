module Main where

import Control.Concurrent.Suspend.Lifted
import Control.Concurrent.Timer
import DBus.Notify
import Data.List
import Data.Time
import Data.Time.Format
import System.Directory
import System.Environment
import System.IO

data Task = Task { description :: String
                 , cost :: Int
                 , deadline :: Day
                 } 
instance Show Task where
    show task = (description task)
                ++ ", " ++ (show . cost $ task)
                ++ ", " ++ (formatTime defaultTimeLocale "%d.%m.%Y" $ deadline task)

instance Eq Task where
    a == b =    description a == description b 
             && cost        a == cost        b 
             && deadline    a == deadline    b

makeTask :: String -> Task
makeTask line = let f = span (/= ',') line
                    s = span (/= ',') . drop 1 $ snd f
                    t = span (/= ',') . drop 1 $ snd s
                in Task { description = fst f
                        , cost = read (fst s) :: Int
                        , deadline = parseTimeOrError True defaultTimeLocale "%d.%m.%Y" $ fst t :: Day
                        }

getPriority :: Day -> Task -> Integer
getPriority dl task = let numenator = (toInteger . (*3) . cost $ task)
                          denominator = (diffDays (deadline task) dl)
                      in if denominator > 0 
                            then numenator `div` denominator
                            else toInteger (cost task ^ 3) + abs denominator

getTopTask :: String -> IO Task
getTopTask filename = do
    content <- readFile filename
    utcCurrDay <- getCurrentTime
    let currDay = utctDay utcCurrDay
        tasks = makeTasks . lines $ content
        priority = getPriority currDay
     in return $ foldl1 (\acc task -> if priority acc < priority task
                                                    then task
                                                    else acc) tasks

makeTasks :: [String] -> [Task]
makeTasks = map makeTask . filter (/= "")

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("view", view)
            , ("add", add)
            , ("remove", remove)
            , ("startReminder", startReminder)
            ]

view :: [String] -> IO ()
view [filename] = do
    task <- getTopTask filename
    putStrLn $ description task

remove :: [String] -> IO()
remove [filename] = do
    handle <- openFile filename ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    task <- getTopTask filename
    let tasks = makeTasks . lines $ contents
        newTodo = delete task tasks
    hPutStrLn tempHandle $ unlines . map show $ newTodo
    hClose handle
    hClose tempHandle
    removeFile filename
    renameFile tempName filename

add :: [String] -> IO()
add [filename] = do
    putStrLn "Print new task: "
    line <- getLine
    let newTodo = show . makeTask $ line
    appendFile filename (newTodo ++ "\n")

startReminder :: [String] -> IO()
startReminder [filename] = do
    task <- getTopTask filename
    repeatedTimer (remind task) (mDelay 5)
    return ()

remind :: Task -> IO()
remind task = do
    client <- connectSession
    note <- return blankNote { appName = "tasker"
                             , summary = "Reminding"
                             , body = Just . Text . description $ task }
    notify client note
    return ()

main :: IO ()
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

