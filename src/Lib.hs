module Lib where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import Data.Csv.Streaming

type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

baseballStats :: BL.ByteString -> Records BaseballStats
baseballStats = decode NoHeader

fourth :: (a, b, c, d) -> d
fourth (_, _, _, d) = d

summer :: (a, b, c, Int) -> Int -> Int
summer = (+) . fourth

-- FilePath is just an alias for String
getAtBatsSum :: FilePath -> IO Int
getAtBatsSum battingCsv = do
  csvData <- BL.readFile battingCsv
  return $ F.foldr summer 0 (baseballStats csvData)

-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Foldable as F
-- -- from cassava
-- import Data.Csv.Streaming
-- -- This is for non streaming option
-- --import Data.Csv


-- -- a simple type alias for data
-- type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)
-- baseballStats :: BL.ByteString -> Either String (V.Vector BaseballStats)
-- baseballStats = decode NoHeader

-- fourth :: (a, b, c, d) -> d
-- fourth (_, _, _, d) = d

-- main :: IO ()
-- main = do
--   csvData <- BL.readFile "batting.csv"
--   -- Either left is NoHeader and Either right is the csvData 
--   let v = decode NoHeader csvData :: Either String (V.Vector BaseballStats)
--   -- Flatmap reduce operation to compute the sum of bats
--   -- Non streaming option
--   --let summed = fmap (V.foldr summer 0) (baseballStats csvData)
--   let summed = F.foldr summer 0 (baseballStats csvData)
--   putStrLn $ "Total atBats was: " ++ (show summed)
--   -- summer maps to the types in BaseballStats
--   -- Point free style https://www.haskell.org/haskellwiki/Pointfree
--   -- The entire definition of . is:
--   --   (f . g) x = f (g x)
--   -- apply forth to r and the do add using the + on the n
--   where summer = (+) . fourth
--   -- where summer r n = n + fourth r
--   -- where summer (name, year, team, atBats) n = n + atBats
--   -- or  where summer (_, _, _, atBats) n = n + atBats



-- Some more examples
-- -- Person is a product/record, if that
-- -- is confusing think "struct" but better.
-- Prelude> data Person = Person String Int String deriving Show

-- Prelude> :type Person
-- Person :: String -> Int -> String -> Person

-- Prelude> :t Person "Chris" 415
-- Person "Chris" 415 :: String -> Person

-- Prelude> :t Person "Chris" 415 "Allen"
-- Person "Chris" 415 "Allen" :: Person

-- Prelude> let namedChris = Person "Chris"
-- Prelude> namedChris 415 "Allen"
-- Person "Chris" 415 "Allen"

-- Prelude> Person "Chris" 415 "Allen"
-- Person "Chris" 415 "Allen"

-- Prelude> let v = Right 1 :: Either String Int
-- Prelude> let x = Left "blah" :: Either String Int

-- Prelude> :t v
-- v :: Either String Int
-- Prelude> :t x
-- x :: Either String Int

-- Prelude> let addOne x = x + 1
-- <interactive>:4:12: Warning:
--     This binding for ‘x’ shadows the existing binding
--       defined at <interactive>:3:5
-- Prelude> addOne 2
-- <interactive>:5:1: Warning:
--     Defaulting the following constraint(s) to type ‘Integer’
--       (Show a0) arising from a use of ‘print’ at <interactive>:5:1-8
--       (Num a0) arising from a use of ‘it’ at <interactive>:5:1-8
--     In a stmt of an interactive GHCi command: print it
-- 3

-- Prelude> fmap addOne v
-- Right 2
-- Prelude> fmap addOne x
-- Left "blah"

-- 

-- addOne :: Int -> Int
-- addOne x = x + 1 -- at least we can abstract this out

-- incrementEither :: Either e Int -> Either e Int
-- incrementEither (Right numberWeWanted) = Right (addOne numberWeWanted)
-- incrementEither (Left errorString) = Left errorString

-- Prelude> :t foldr
-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- Prelude> foldr (+) 0 [1, 2, 3]
-- 6
-- Prelude> foldr (+) 1 [1, 2, 3]
-- 7
-- Prelude> foldr (+) 2 [1, 2, 3]
-- 8
-- Prelude> foldr (+) 2 [1, 2, 3, 4]
-- 12

-- Prelude> :t (++)
-- (++) :: [a] -> [a] -> [a]

-- Prelude> [1, 2, 3] ++ [4, 5, 6]
-- [1,2,3,4,5,6]
-- Prelude> "hello, " ++ "world!"
-- "hello, world!"
