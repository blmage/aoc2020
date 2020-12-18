{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}

{-|
Module      : Day05
Description : Binary Boarding
Copyright   : (c) blmage 2020
License     : MIT
-}
module Day05
       (
         -- * Puzzle 1
         -- $puzzle1
         maximum

         -- * Puzzle 2
         -- $puzzle2
       , findMissingSeatId

         -- * Global types and functions
       , Seat
       , SeatId
       , Position
       , Instruction
       , seatParser
       , sequenceParser
       ) where

import Control.Monad (foldM)
import Data.Foldable (maximum)
import Data.List.GroupBy (groupBy)
import Lens.Micro (_head)
import Lens.Micro.Extras (preview)
import Text.Parser.Char (char)

import Days


instance Solved Day5 where
    solution puzzle = withParsedInputLineList Day5 seatParser
        $ pure
        . show
        . (go . fmap seatId)
      where
        go :: [SeatId] -> Maybe SeatId
        go = case puzzle of
            Puzzle1 -> Just . maximum
            Puzzle2 -> findMissingSeatId


-- | A position on an axis.
data Position
    = Start
    | End

-- | A positioning instruction.
data Instruction (position :: Position) where
    B :: Instruction 'End
    F :: Instruction 'Start
    L :: Instruction 'Start
    R :: Instruction 'End

-- | A seat in a plane.
data Seat
    = Seat
    { -- | The row where the seat is located.
      seatRow    :: !Int
    , -- | The column where the seat is located.
      seatColumn :: !Int
    }

-- | The ID of a seat.
newtype SeatId = SeatId { unSeatId :: Int }
    deriving newtype (Bounded, Enum, Eq, Ord, Show)


-- | A parser for a 'Seat', based on two sequences of 'Instruction's.
seatParser :: Parser Seat
seatParser = do
    seatRow    <- sequenceParser f b 7
    seatColumn <- sequenceParser l r 3
    pure Seat {..}
  where
    f, l :: Parser (Instruction 'Start)
    f = char 'F' $> F
    l = char 'L' $> L

    b, r :: Parser (Instruction 'End)
    b = char 'B' $> B
    r = char 'R' $> R

-- | A parser for a sequence of @n@ 'Instruction's relative to an axis.
sequenceParser
    :: Parser (Instruction 'Start)
    -> Parser (Instruction 'End)
    -> Int
    -> Parser Int
sequenceParser start end count
    = foldM (instructionParser start end) 0
    $ reverse $ (2 ^) <$> [0..count-1]
  where
    instructionParser
        :: Parser (Instruction 'Start)
        -> Parser (Instruction 'End)
        -> Int
        -> Int
        -> Parser Int
    instructionParser start end base offset =
        (start $> Start <|> end $> End) <&> \case
            Start -> base
            End   -> base + offset


-- | Returns the 'SeatId' of a given 'Seat'.
seatId :: Seat -> SeatId
seatId Seat {..} = SeatId $ seatRow * 8 + seatColumn

-- | Returns whether two 'SeatId's correspond to adjacent 'Seat's.
isAdjacentSeatIds :: SeatId -> SeatId -> Bool
isAdjacentSeatIds = (\x y -> abs (x - y) == 1) `on` unSeatId

-- | Returns the first missing 'SeatId' from the given list, if any.
findMissingSeatId :: [SeatId] -> Maybe SeatId
findMissingSeatId
    = fmap prev
    . preview (_head . _head)
    . drop 1
    . groupBy isAdjacentSeatIds
    . sort


{- $puzzle1
== --- Day 5: Binary Boarding ---

You board your plane only to discover a new problem: you dropped your boarding pass! You
aren\'t sure which seat is yours, and all of the flight attendants are busy with the
flood of people that suddenly made it through passport control.

You write a quick program to use your phone\'s camera to scan all of the nearby boarding
passes (your puzzle input); perhaps you can find your seat through process of
elimination.

Instead of <https://www.youtube.com/watch?v=oAHbLRjF0vo zones or groups>, this airline
uses /binary space partitioning/ to seat people. A seat might be specified like
@FBFBBFFRLR@, where @F@ means \"front\", @B@ means \"back\", @L@ means \"left\", and @R@
means \"right\".

The first 7 characters will either be @F@ or @B@; these specify exactly one of the /128
rows/ on the plane (numbered @0@ through @127@). Each letter tells you which half of a
region the given seat is in. Start with the whole list of rows; the first letter
indicates whether the seat is in the /front/ (@0@ through @63@) or the /back/ (@64@
through @127@). The next letter indicates which half of that region the seat is in, and
so on until you\'re left with exactly one row.

For example, consider just the first seven characters of @FBFBBFFRLR@:

-   Start by considering the whole range, rows @0@ through @127@.
-   @F@ means to take the /lower half/, keeping rows @0@ through @63@.
-   @B@ means to take the /upper half/, keeping rows @32@ through @63@.
-   @F@ means to take the /lower half/, keeping rows @32@ through @47@.
-   @B@ means to take the /upper half/, keeping rows @40@ through @47@.
-   @B@ keeps rows @44@ through @47@.
-   @F@ keeps rows @44@ through @45@.
-   The final @F@ keeps the lower of the two, /row @44@/.

The last three characters will be either @L@ or @R@; these specify exactly one of the /8/
/columns/ of seats on the plane (numbered @0@ through @7@). The same process as above
proceeds again, this time with only three steps. @L@ means to keep the /lower half/,
while @R@ means to keep the /upper half/.

For example, consider just the last 3 characters of @FBFBBFFRLR@:

-   Start by considering the whole range, columns @0@ through @7@.
-   @R@ means to take the /upper half/, keeping columns @4@ through @7@.
-   @L@ means to take the /lower half/, keeping columns @4@ through @5@.
-   The final @R@ keeps the upper of the two, /column @5@/.

So, decoding @FBFBBFFRLR@ reveals that it is the seat at /row @44@, column @5@/.

Every seat also has a unique /seat ID/: multiply the row by 8, then add the column. In
this example, the seat has ID @44 * 8 + 5 = 357@.

Here are some other boarding passes:

-   @BFFFBBFRRR@: row @70@, column @7@, seat ID @567@.
-   @FFFBBBFRRR@: row @14@, column @7@, seat ID @119@.
-   @BBFFBBFRLL@: row @102@, column @4@, seat ID @820@.

As a sanity check, look through your list of boarding passes. /What is the highest seat/
/ID on a boarding pass?/
-}

{- $puzzle2
== --- Part Two ---
#part2#

/Ding!/ The \"fasten seat belt\" signs have turned on. Time to find your seat.

It\'s a completely full flight, so your seat should be the only missing boarding pass in
your list. However, there\'s a catch: some of the seats at the very front and back of the
plane don\'t exist on this aircraft, so they\'ll be missing from your list as well.

Your seat wasn\'t at the very front or back, though; the seats with IDs +1 and -1 from
yours will be in your list.

/What is the ID of your seat?/
-}
