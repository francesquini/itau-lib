module Itau.Utils (
      withEcho
    , getUserInput
    , today
    , comb
    , fracEquals
    , fracEquals2
    , fromItauNum
    , fromItauDate
    , isBlank
    , ItauDateDirection (Forward, Backward, Current)
) where

import           Control.Exception
import           Control.Monad
import qualified Data.Foldable as F
import           Data.List.Split
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Calendar
import           Data.Text (Text)
import           GHC.IO.Handle
import           System.IO

isBlank :: Text -> Bool
isBlank = T.null . T.strip

today :: IO Day
today = do
    now <- getCurrentTime >>= utcToLocalZonedTime
    return $ localDay $ zonedTimeToLocalTime now

comb :: Int -> [a] -> [[a]]
comb m xs = combsBySize xs !! m
    where
        combsBySize = foldr partial ([[]] : repeat [])
        partial x next = zipWith (++) (map (map (x:)) ([]:next)) next

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

getUserInput :: String -> Bool -> IO String
getUserInput text echo = do
    putStr text
    hFlush stdout
    ret <- withEcho echo getLine
    unless echo $ putChar '\n'
    return ret

fracEquals :: Float -> Float  -> Float  -> Bool
fracEquals maxDiff a b  = abs (a - b) <= maxDiff

fracEquals2 :: Float -> Float  -> Bool
fracEquals2 = fracEquals 0.01

fromItauNum :: Text -> Float
fromItauNum val =
        if '-' `elem` T.unpack val
            then negate num
            else num
    where
        el = (`F.elem`  (",0123456789":: String))
        num = read . T.unpack . T.replace "," "." $ T.filter el val


data ItauDateDirection = Forward | Current | Backward

yearGivenDirection :: Integer -> Int -> Int -> ItauDateDirection -> Integer
yearGivenDirection curYear _        _     Current  = curYear
yearGivenDirection curYear curMonth month Forward  = if month < curMonth then curYear + 1 else curYear
yearGivenDirection curYear curMonth month Backward = if month > curMonth then curYear - 1 else curYear

fromItauDate :: Day -> ItauDateDirection -> Text -> Day
fromItauDate day direction t =
    pDate $ splitOn "/" (T.unpack t)
    where
        (year, month, _) = toGregorian day
        pDate [d, m] =
            fromGregorian (yearGivenDirection year month (read m) direction) (read m) (read d)
        pDate [d, m, y] = fromGregorian (read y) (read m) (read d)
        pDate x = error $ show x
