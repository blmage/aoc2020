{-|
Module      : Day01
Description : Report Repair
Copyright   : (c) blmage 2020
License     : MIT
-}
module Day01
       (
         -- * Puzzle 1
         -- $puzzle1
         findYearPair

         -- * Puzzle 2
         -- $puzzle2
       , findYearTriplet
       ) where

import Data.Foldable (asum, find)
import Data.List (tails, unfoldr)
import Text.Megaparsec ()
import Text.Parser.Token (decimal)

import Days


instance Solved Day1 where
    solution = \case
        Puzzle1 -> go $ findYearPair    2020
        Puzzle2 -> go $ findYearTriplet 2020
      where
        go f
            = withParsedInputLineList Day1 decimal
            $ maybeToError "No result for the given list."
            . fmap show
            . f


-- | Finds the pair of 'Integer's in the given list which sums up to the given year.
--
-- Returns the corresponding product.
--
-- >>> findYearPair 2020 [1721, 979, 366, 299, 675, 1456]
-- Just 514579
--
-- >>> findYearPair 2020 [1..3]
-- Nothing
findYearPair :: Integer -> [Integer] -> Maybe Integer
findYearPair year = \case
    xs@(_ : ys@(_ : _)) -> asum $ zipWith go xs (tails ys)
    _ -> Nothing
  where
    go :: Integer -> [Integer] -> Maybe Integer
    go x = fmap (* x) . find ((== year) . (+ x))

-- | Finds the triplet of 'Integer's in the given list which sums up to the given year.
--
-- Returns the corresponding product.
--
-- >>> findYearTriplet 2020 [1721, 979, 366, 299, 675, 1456]
-- Just 241861950
--
-- >>> findYearTriplet 2020 [1..3]
-- Nothing
findYearTriplet :: Integer -> [Integer] -> Maybe Integer
findYearTriplet year = \case
    xs@(_ : ys@(_ : _ : _))-> asum $ zipWith go xs (tails ys)
    _ -> Nothing
  where
    go :: Integer -> [Integer] -> Maybe Integer
    go x = fmap (* x) . findYearPair (year - x)


{- $puzzle1
== --- Day 1: Report Repair ---

After saving Christmas </events five years in a row>, you\'ve decided to take a vacation
at a nice resort on a tropical island. Surely, Christmas will go on without you.

The tropical island has its own currency and is entirely cash-only. The gold coins used
there have a little picture of a starfish; the locals just call them /stars/. None of the
currency exchanges seem to have heard of them, but somehow, you\'ll need to find fifty of
these coins by the time you arrive so you can pay the deposit on your room.

To save your vacation, you need to get all /fifty stars/ by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day in the
Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle
grants /one star/. Good luck!

Before you leave, the Elves in accounting just need you to fix your /expense report/
(your puzzle input); apparently, something isn\'t quite adding up.

Specifically, they need you to /find the two entries that sum to @2020@/ and then
multiply those two numbers together.

For example, suppose your expense report contained the following:

> 1721
> 979
> 366
> 299
> 675
> 1456

In this list, the two entries that sum to @2020@ are @1721@ and @299@. Multiplying them
together produces @1721 * 299 = 514579@, so the correct answer is @514579@.

Of course, your expense report is much larger. /Find the two entries that sum to @2020@;/
/what do you get if you multiply them together?/

-}

{- $puzzle2
== --- Part Two ---
#part2#

The Elves in accounting are thankful for your help; one of them even offers you a
starfish coin they had left over from a past vacation. They offer you a second one if you
can find /three/ numbers in your expense report that meet the same criteria.

Using the above example again, the three entries that sum to @2020@ are @979@, @366@, and
@675@. Multiplying them together produces the answer, @241861950@.

In your expense report, /what is the product of the three entries that sum to @2020@?/
-}
