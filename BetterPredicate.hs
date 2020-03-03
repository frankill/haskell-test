module BetterPredicate  where

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions,searchable)
-- import System.Time (ClockTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle,SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

-- the function we wrote earlier
import RecursiveContents (getRecursiveContents)

type InfoP a =  FilePath        -- path to directory entry
            -> Permissions     -- permissions
            -> Maybe Integer   -- file size (Nothing if not file)
            -- -> ClockTime       -- last modified
            -> a

pathP :: InfoP FilePath
pathP path _ _  = path

permP :: InfoP Bool
permP _ perms _ = searchable perms

sizeP :: InfoP Integer
sizeP _ _ (Just size)  = size
sizeP _ _ Nothing      = -1

-- equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
-- equalP f k = \w x y  -> f w x y  == k

-- equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
-- equalP f k w x y = f w x y == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y  = f w x y `q` k

equalP, greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)
equalP = liftP (==)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y  = f w x y  `q` g w x y 

andP,orP :: InfoP a -> InfoP b -> InfoP Bool
andP = liftP2 (&&)
orP = liftP2 (||)

-- andP (sizeP `equalP` 1024) (pathP `equalP` "abc") 

-- liftPath :: (FilePath -> a) -> InfoP a
-- liftPath f w _ _ _ = f w

-- myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
--          (sizeP `greaterP` 131072)

(==?) = equalP
(&&?) = andP
(>?) = greaterP
(<?) = lesserP
(||?) = orP

 -- (sizeP >? 1024) ||? (pathP ==? "abc") 
--myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

err ::  SomeException -> IO (Maybe Integer)
err _ =  return Nothing 

getFileSize ::  FilePath -> IO (Maybe Integer)
getFileSize path = handle err $ do 
    bracket (openFile path ReadMode) hClose $ \h ->  
        hFileSize h >>= (\x -> return (Just x))  

betterFind :: InfoP Bool -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
   where check name = do
           perms <- getPermissions name
           size <- getFileSize name
           -- modified <- getModificationTime name
           return (p name perms size)
