module Parser(splitLines, parseMake, SourceFileMap(..), MakeTargetMap(..)) where

import qualified Data.Map.Strict as StrictMap (Map)
import Data.Char (isSpace, isAlpha, isAlphaNum)

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

parseIdentifier :: String -> Maybe String
parseIdentifier str  = let allowed = (\x -> isAlphaNum x || (elem x ['_','/']))
                       in case allowed (head str) of
                          True ->  Just $ takeWhile allowed str 
                          False -> Nothing

parseWhiteSpace :: String -> Maybe String
parseWhiteSpace str | isSpace (head str) = Just $ takeWhile isSpace str
                    | otherwise          = Nothing

parseOperator :: String -> Maybe String
parseOperator str = let isOperator x = elem x ['+','=',':','-','*','/']
                        isOperatorOrSpace x = (isOperator x) || (isSpace x)
                    in if isOperator (head str) then Just $ takeWhile isOperatorOrSpace str else Nothing

parseRest :: String -> Maybe String
parseRest str | isSpace (head str) = Nothing
              | otherwise          = Just $ takeWhile (not . isSpace) str

parseMake :: MakeTargetMap -> SourceFileMap -> String -> (MakeTargetMap, SourceFileMap)
parseMake a b _ = (a,b)  

-- regex-tdfa Abhängigkeit
-- Folgende Vorgehensweise:
-- import Text.Regex.Posix
-- let matchResult = "my_foolib_CXXSRCS" =~ "^[ \t]*([A-Za-z0-9_]+)_(C|CXX)?SRCS" :: MatchResult String
-- man kann dann mrSubList matchResult abfragen. Falls die Liste leer => kein Match, ansonsten ist das erste Element der MatchString
-- let (a,b,c,d) = "my_foolib_CXXSRCS" =~ "([A-Za-z0-9_]*)_(C|CXX)?SRCS" :: (String, String, String, [String])
--
