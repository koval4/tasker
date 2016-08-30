module Main where

import DBus.Notify
import Data.List
import Data.List.Split
import Data.Time
import System.Directory
import System.Environment
import System.IO
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

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
makeTask line = let a:b:c:_ = splitOn "," line
                in Task { description = a
                        , cost = read b :: Int
                        , deadline = parseTimeOrError True defaultTimeLocale "%d.%m.%Y" c :: Day
                        }

getPriority :: Day -> Task -> Integer
getPriority dl task = let numenator = (toInteger . (*3) . cost $ task)
                          denominator = (diffDays (deadline task) dl)
                      in if denominator > 0
                            then numenator `div` denominator
                            else toInteger (cost task ^ 3) + abs denominator

getTopTask :: String -> MaybeT IO Task
getTopTask filename = do
    content <- lift $ readFile filename
    guard (null content)
    utcCurrDay <- lift getCurrentTime
    let currDay = utctDay utcCurrDay
        tasks = makeTasks . lines $ content
        priority = getPriority currDay
      in return $ foldl1 (\acc task -> if priority acc < priority task
                                                 then task
                                                 else acc) tasks

makeTasks :: [String] -> [Task]
makeTasks = map makeTask . filter (/= "")

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("view", unwrapT view)
            , ("add", add)
            , ("remove", unwrapT remove)
            , ("startReminder", unwrapT startReminder)
            ]
  where unwrapT fn args = runMaybeT (fn args) >> return ()

view :: [String] -> MaybeT IO ()
view [filename] = do
    task <- getTopTask filename
    lift $ putStrLn $ description task

remove :: [String] -> MaybeT IO()
remove [filename] = do
    handle <- lift $ openFile filename ReadMode
    (tempName, tempHandle) <- lift $ openTempFile "." "temp"
    contents <- lift $ hGetContents handle
    task <- getTopTask filename
    let tasks = makeTasks . lines $ contents
        newTodo = delete task tasks
    lift $ do
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

startReminder :: [String] -> MaybeT IO()
startReminder [filename] = do
    task <- getTopTask filename
    lift $ forever $ do
        let delay = 20 :: Int
        remind task
        threadDelay (delay * 60 * 1000)

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

