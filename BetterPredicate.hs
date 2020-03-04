module BetterPredicate  where

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions,searchable)
import System.Time (ClockTime(..))
import Control.Exception (bracket, handle,SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import RecursiveContents (getRecursiveContents)

data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe ClockTime
    } deriving (Eq, Ord, Show)

liftP :: (a -> b -> c) -> (Info -> a) -> a -> Info  -> Bool
liftP q f k d  = f d `q` k

(==?), (<?), (>?) :: (Ord a) => (Info -> a) -> a -> Info -> Bool
(>?) = liftP (>)
(<?) = liftP (<)
(==?) = liftP (==)

liftP2 :: (a -> b -> c) -> (Info -> a) -> (Info -> b) -> Info -> Bool
liftP2 q f g d = f d `q` g d

(&&?),(||?) :: (Info -> a) -> (Info -> b) -> Info -> Bool
(&&?) = liftP2 (&&)
(||?) = liftP2 (||)
 
err ::  SomeException -> IO (Maybe Integer)
err _ =  return Nothing 

getFileSize ::  FilePath -> IO (Maybe Integer)
getFileSize path = handle err $ do 
    bracket (openFile path ReadMode) hClose $ \h ->  
        hFileSize h >>= (\x -> return (Just x))  

betterFind :: (Info -> Bool ) -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check 
    where 
		check name = do
				   perms <- getPermissions name
				   size <- getFileSize name
				   modified <- getModificationTime name
				   return (p $ Info name (Just perms) size (Just modified))
