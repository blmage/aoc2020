{-# LANGUAGE GADTs #-}

{-|
Module      : Day03
Description : Toboggan Trajectory
Copyright   : (c) blmage 2020
License     : MIT
-}
module Day03
       (
         -- * Puzzle 1
         -- $puzzle1
         hasTreeAt
       , countTreesOnSlope

         -- * Puzzle 2
         -- $puzzle2
       , countTreesOnSlopes

         -- * Global types and functions
       , Tile
       , RowPattern
       , Forest
       , rowPatternParser
       ) where

import Control.Conditional ((|>))
import Data.Vector.Sized (Vector)
import Text.Parser.Char (char)

import Days

import qualified Data.Finite as Fin
import qualified Data.Vector.Sized as V


instance Solved Day3 where
    solution puzzle
        = withParsedInputLineList Day3 rowPatternParser
        $ pure
        . show
        . go
        . Forest
      where
        go :: Forest -> Integer
        go = case puzzle of
            Puzzle1 ->
                countTreesOnSlope (3, 1)
            Puzzle2 ->
                product
                    . countTreesOnSlopes
                    [ (1, 1)
                    , (3, 1)
                    , (5, 1)
                    , (7, 1)
                    , (1, 2)
                    ]


-- | A forest tile.
data Tile
    = Open
    | Tree
    deriving Eq

-- | A tile pattern for a forest row, that repeats itself infinitely.
data RowPattern where
    RowPattern :: forall width. KnownNat width => Vector width Tile -> RowPattern

-- | A forest.
newtype Forest = Forest { unForest :: [RowPattern] }


-- | A 'Parser' for a 'RowPattern'.
rowPatternParser :: Parser RowPattern
rowPatternParser = do
    tiles <- some (char '.' $> Open <|> char '#' $> Tree)
    pure $ V.withSizedList tiles RowPattern


-- | Returns whether there is a 'Tree' at a given index in a 'Forest' row based on the
-- given 'RowPattern'.
hasTreeAt :: RowPattern -> Integer -> Bool
hasTreeAt (RowPattern tiles) = (== Tree) . V.index tiles . Fin.modulo

-- | Returns the number of trees in the given 'Forest' that are present on a given slope.
countTreesOnSlope :: (Integer, Integer) -> Forest -> Integer
countTreesOnSlope (x, y) (Forest rows)
    = fromIntegral
    $ length
    $ filter id
    $ zipWith (flip hasTreeAt) (iterate (+ x) 0)
    $ catMaybes
    $ zipWith (\n row -> (n `mod` y /= 0) |> row) [0..] rows

-- | Returns the number of trees in the given 'Forest' that are present on each given
-- slope.
countTreesOnSlopes :: [(Integer, Integer)] -> Forest -> [Integer]
countTreesOnSlopes slopes = zipWith countTreesOnSlope slopes . repeat


{- $puzzle1
== --- Day 3: Toboggan Trajectory ---

With the toboggan login problems resolved, you set off toward the airport. While travel
by toboggan might be easy, it\'s certainly not safe: there\'s very minimal steering and
the area is covered in trees. You\'ll need to see which angles will take you near the
fewest trees.

Due to the local geology, trees in this area only grow on exact integer coordinates in a
grid. You make a map (your puzzle input) of the open squares (@.@) and trees (@#@) you
can see. For example:

> ..##.......
> #...#...#..
> .#....#..#.
> ..#.#...#.#
> .#...##..#.
> ..#.##.....
> .#.#.#....#
> .#........#
> #.##...#...
> #...##....#
> .#..#...#.#

These aren\'t the only trees, though; due to something you read about once involving
arboreal genetics and biome stability, the same pattern repeats to the right many times:

> ..##.........##.........##.........##.........##.........##.......  --->
> #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
> .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
> ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
> .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
> ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
> .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
> .#........#.#........#.#........#.#........#.#........#.#........#
> #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
> #...##....##...##....##...##....##...##....##...##....##...##....#
> .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->

You start on the open square (@.@) in the top-left corner and need to reach the bottom
(below the bottom-most row on your map).

The toboggan can only follow a few specific slopes (you opted for a cheaper model that
prefers rational numbers); start by /counting all the trees/ you would encounter for the
slope /right 3, down 1/:

From your starting position at the top-left, check the position that is right 3 and down
1. Then, check the position that is right 3 and down 1 from there, and so on until you go
past the bottom of the map.

The locations you\'d check in the above example are marked here with @O@ where there was
an open square and @X@ where there was a tree:

> ..##.........##.........##.........##.........##.........##.......  --->
> #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
> .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
> ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
> .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
> ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
> .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
> .#........#.#........X.#........#.#........#.#........#.#........#
> #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
> #...##....##...##....##...#X....##...##....##...##....##...##....#
> .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->

In this example, traversing the map using this slope would cause you to encounter @7@
trees.

Starting at the top-left corner of your map and following a slope of right 3 and down 1,
/how many trees would you encounter?/
-}

{- $puzzle2
== --- Part Two ---
#part2#

Time to check the rest of the slopes - you need to minimize the probability of a sudden
arboreal stop, after all.

Determine the number of trees you would encounter if, for each of the following slopes,
you start at the top-left corner and traverse the map all the way to the bottom:

-   Right 1, down 1.
-   Right 3, down 1. (This is the slope you already checked.)
-   Right 5, down 1.
-   Right 7, down 1.
-   Right 1, down 2.

In the above example, these slopes would find @2@, @7@, @3@, @4@, and @2@ tree(s)
respectively; multiplied together, these produce the answer @336@.

/What do you get if you multiply together the number of trees encountered on each of/
/the listed slopes?/
-}
