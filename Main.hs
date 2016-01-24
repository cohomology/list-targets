module Main where

import qualified System.Environment as SysEnv (getArgs) 
import qualified System.Directory as SysDir (doesFileExist)
import Data.Map
import Parser

data UsageError    = NotSingleArgument | ArgumentNoFile 

usage :: UsageError -> IO a
usage x = do case x of 
               NotSingleArgument -> putStrLn "Error: You need to supply exactly one argument."
               ArgumentNoFile    -> putStrLn "Error: First argument is no valid file."
             error "Usage: find_target makefile"

checkArguments :: [String] -> IO String
checkArguments [a] = do success <- SysDir.doesFileExist a 
                        if success then return a else usage ArgumentNoFile
checkArguments _   = usage NotSingleArgument

main :: IO (MakeTargetMap, SourceFileMap)
main = let targetMap = MakeTargetMap empty 
           sourceMap = SourceFileMap empty 
       in do args    <- SysEnv.getArgs
             file    <- checkArguments args 
             content <- readFile file
             return $ parseMake targetMap sourceMap content
