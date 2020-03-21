module Pnm(
    Grepmap(..),
    parseGrepmap,
    parseByte,
    parse 
    ) where 

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char(isSpace)
import Data.Int(Int64)
import Data.Word(Word8)

data Grepmap = Grepmap {
    grepWidth :: Int,
    grepHeght :: Int,
    grepMax   :: Int,
    grepData  :: L.ByteString
} deriving (Eq)

instance Show Grepmap where
    show (Grepmap w h m _) = "Grepmap " ++ show w ++ "x" ++ show h ++  " " ++ show m 

data ParseStatus = ParseStatus {
    string :: L.ByteString,
    offset :: Int64
} deriving(Show)

newtype Parse a = Parse { runParse :: ParseStatus -> Either String (a, ParseStatus) }  

identity :: a  -> Parse a 
identity a = Parse (\s -> Right (a, s))

getState :: Parse ParseStatus
getState = Parse (\s -> Right (s, s))

putState :: ParseStatus -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser  =  Parse chainedParser
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage -> Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState

parseByte :: Parse Word8 
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->  bail "no more input"
      Just (byte,remainder) ->
          putState newState ==> \_ ->
          identity byte
        where newState = initState { string = remainder,
                                     offset = newOffset }
              newOffset = offset initState + 1

parseGrepmap :: Parse Grepmap
parseGrepmap = 
    getState ==> \str -> 
     case parseP5 (string str) of
      Nothing -> bail "no more input"
      Just (byte,remainder,num) ->
            putState newState ==> \_ ->
            identity byte
        where newState = str { string = remainder,
                                     offset = newOffset }
              newOffset = offset str + num

parse :: Parse a -> L.ByteString -> Either String a 
parse parser initData = 
    case runParse parser (ParseStatus initData 0) of 
        Left err -> Left err 
        Right (a , _) -> Right a 

parseP5 :: L.ByteString ->  Maybe (Grepmap, L.ByteString , Int64)
parseP5 str = 
    matchHeader (L8.pack "P5") str       >>?
    \s ->  skipSpace ((),s)              >>?
    (getNat . snd)                       >>?
    skipSpace                            >>?
    \(width ,s1) -> getNat s1            >>?
    skipSpace                            >>?
    \(height ,s2) -> getNat s2           >>?
    skipSpace                            >>?
    \(maxGrep,s3) -> getByte (width * height) s3        >>? 
    \(bitmap, s4) -> Just (Grepmap width height maxGrep bitmap, s4, fromIntegral(width * height + 4))


(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just a >>? f = f a

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str 
    | prefix `L8.isPrefixOf` str = Just (L8.drop (L8.length prefix) str)
    | otherwise = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of 
                Nothing -> Nothing
                Just(num, rest) 
                    | num <= 0 -> Nothing
                    | otherwise -> Just(num,rest)

getByte :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getByte n str = let count = fromIntegral n
                    both@(prefix, _) = L.splitAt count str
                in if L.length prefix < count 
                   then Nothing
                   else Just both 
