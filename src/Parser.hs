module Parser(splitLines, parseMake, SourceFileMap(..), MakeTargetMap(..)) where

import qualified Data.Map.Strict as StrictMap (Map)
import Data.Char (isSpace)

type TargetString  = String
type Library       = String
type SourceFile    = String
newtype MakeTargetMap = MakeTargetMap (StrictMap.Map Library TargetString)
newtype SourceFileMap = SourceFileMap (StrictMap.Map SourceFile Library)

-- |Splits a makefile into lines, removing comments and concatenating subsequent lines
-- if the last character in the line is a backslash (not escaped by another backslash)
splitLines :: String -> [String]                                              
splitLines str = doSplit (dropWhile isSpace str) [] False 
                 where handleComment a b False = a ++ b
                       handleComment a _ True = a
                       doSplit :: String -> String -> Bool -> [String]
                       doSplit []             u _     = [u]
                       doSplit ('\\':'\\':xs) u c     = doSplit xs (handleComment u "\\" c) c
                       doSplit ('\\':'\n':xs) u c     = doSplit xs u c
                       doSplit ('\n':xs)      u _     = [u] ++ doSplit (dropWhile isSpace xs) [] False
                       doSplit ('\\':'#':xs)  u c     = doSplit xs (handleComment u "#" c) c
                       doSplit ('#':xs)       u _     = doSplit xs u True
                       doSplit (x:xs)         u c     = doSplit xs (handleComment u [x] c) c 

parseMake :: MakeTargetMap -> SourceFileMap -> String -> (MakeTargetMap, SourceFileMap)
parseMake a b _ = (a,b)  
