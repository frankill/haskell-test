-- 对某个模式的匹配从字符串头部开始，在字符串尾部结束。
-- 多数文本字符匹配自身。例如，文本 foo 作为模式匹配其自身 foo ，且在一个输入字符串中仅匹配 foo 。
-- (星号) 意味着 “匹配所有”; 其将匹配所有文本，包括空字符串。 例如, 模式 foo 将匹配任意以 foo 开头的字符串，比如 foo 自身， foobar ， 或 foo.c 。 模式 quux.c 将匹配任何以 quux 开头且以 .c 结束的字符串，如 quuxbaz.c 。
-- ? (问号) 匹配任意单个字符。模式 pic??.jpg 将匹配类似 picaa.jpg 或 pic01.jpg 的文件名。
-- [ (左方括号) 将开始定义一个字符类，以 ] 结束。其意思是 “匹配在这个字符类中的任意字符”。 [! 开启一个否定的字符类，其意为 “匹配不在这个字符类中的任意字符”。
-- 用 - (破折号) 连接的两个字符，是一种表示范围的速记方法，表示：“匹配这个围内的任意字符”。
--
-- 字符类有一个附加的条件；其不可为空。在 [ 或 [! 后的字符是这个字符类的一部分，所以我们可以编写包含 ] 的字符类，如 []aeiou] 。模式 pic[0-9].[pP][nN][gG] 将匹配由字符串 pic 开始，跟随单个数字，最后是字符串 .png 的任意大小写形式。

module Glob
    (
    matchesGlob,
    namesMatchings
    ) where

import Text.Regex.Posix ((=~))
import Data.Char(toUpper)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
import Control.Exception (handle, SomeException)
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, doesFileExist,  getCurrentDirectory, getDirectoryContents)

type Ignore = Bool

matchesGlob  :: FilePath -> String -> Ignore -> Bool
matchesGlob f str ignore
    | ignore = (map toUpper f) =~ (glob . map toUpper $ str)
    | otherwise =  f =~ glob str

namesMatchings :: [FilePath] -> IO [  [String] ]
namesMatchings xs = forM xs namesMatching

namesMatching :: FilePath -> IO [String]
namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return (if exists then [pat] else [])
  | otherwise = do
  case splitFileName pat of
    ("", baseName) -> do
        curDir <- getCurrentDirectory
        listMatches curDir baseName
    (dirName, baseName) -> do
        dirs <- if isPattern dirName
                then namesMatching (dropTrailingPathSeparator dirName)
                else return [dirName]
        let listDir = if isPattern baseName
                      then listMatches
                      else listPlain
        pathNames <- forM dirs $ \dir -> do
                         baseNames <- listDir dir baseName
                         return (map (dir </>) baseNames)
        return (concat pathNames)

doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
      then return True
      else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (const (return [])::(SomeException->IO [String]))
           $ do names <- getDirectoryContents dirName'
                let names' = if isHidden pat
                             then filter isHidden names
                             else filter (not . isHidden) names
                return (filter (=~ (glob pat) ) names' )

listMatches topdir "**" = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
        paths <- forM properNames $ \name ->
        do
          let path = topdir </> name
              isDirectory <- doesDirectoryExist path
              if isDirectory
              then getRecursiveContents path
              else return [path]
          return (concat paths)


isHidden ('.':_) = True
isHidden _       = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])

glob :: String -> String
glob str = '^' : glob' str ++ "$"

glob' :: String -> String
glob' "" = ""
glob' ('*':xs) = ".*" ++ glob' xs
glob' ('?':xs) = "." ++ glob' xs
glob' ('[':'!':x:xs) = "[^" ++ x : charClass xs
glob' ('[':x:xs) = "[" ++ x : charClass xs
glob' ('[':_)        = error "unterminated character class"
glob' (x:xs) = escape x ++  glob' xs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : glob' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"

isPattern :: String -> Bool
isPattern = any (`elem` "[?*")
