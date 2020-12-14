{-# LANGUAGE NamedFieldPuns #-}

{-|
Module      : Day02
Description : Password Philosophy
Copyright   : (c) blmage 2020
License     : MIT
-}
module Day02
       (
         -- * Puzzle 1
         -- $puzzle1
         isValidOldDatabaseRow

         -- * Puzzle 2
         -- $puzzle2
       , isValidNewDatabaseRow

         -- * Global types and functions
       , Password(..)
       , PasswordRule(..)
       , DatabaseRow(..)
       , databaseRowParser
       ) where

import Control.Arrow ((&&&))
import Text.Parser.Char (char, lower)
import Text.Parser.Combinators (some)
import Text.Parser.Token (decimal, someSpace)

import Days


instance Solved 'Day2 where
    solution puzzle
        = withParsedInputLineList Day2 databaseRowParser
        $ pure
        . show
        . length
        . filter isValidDatabaseRow
      where
        isValidDatabaseRow :: DatabaseRow -> Bool
        isValidDatabaseRow = case puzzle of
            Puzzle1 -> isValidOldDatabaseRow
            Puzzle2 -> isValidNewDatabaseRow


-- | A password.
newtype Password = Password { unPassword :: String }

-- | A validation rule for 'Password's.
data PasswordRule
    = PasswordRule
    { -- | The character whose presence in a 'Password' must be tested.
      ruleCharacter :: !Char
    , -- | How many times the tested character must be present _at least_.
      ruleMinCount  :: !Int
    , -- | How many times the tested character must be present _at most_.
      ruleMaxCount  :: !Int
    }

-- | A row from a password database.
data DatabaseRow
    = DatabaseRow
    { -- | A password.
      rowPassword :: !Password
    , -- | The validation rule applied to the 'Password'.
      rowRule     :: !PasswordRule
    }


-- | A 'Parser' for a row of a password database.
databaseRowParser :: Parser DatabaseRow
databaseRowParser = do
    ruleMinCount  <- boundParser
    char '-'
    ruleMaxCount  <- boundParser
    someSpace
    when (ruleMinCount > ruleMaxCount) $ fail "Invalid bounds (min > max)."
    ruleCharacter <- lower
    char ':' *> someSpace
    rowPassword   <- Password <$> some lower
    let rowRule = PasswordRule {..}
    pure $ DatabaseRow {..}
  where
    boundParser :: Parser Int
    boundParser = do
        bound <- decimal
        when (bound > fromIntegral maxInt) $ fail "Invalid bound (too large)."
        pure $ fromInteger bound


-- | Returns whether the given 'DatabaseRow' is valid according to the old policy.
--
-- >>> isValidOldDatabaseRow (DatabaseRow (PasswordRule 'a' 1 3) (Password "abcde"))
-- True
--
-- >>> isValidOldDatabaseRow (DatabaseRow (PasswordRule 'b' 1 3) (Password "cdefg"))
-- False
isValidOldDatabaseRow :: DatabaseRow -> Bool
isValidOldDatabaseRow DatabaseRow { rowRule = PasswordRule {..}, rowPassword }
    = ((>= ruleMinCount) &&^ (<= ruleMaxCount))
    $ length
    $ filter (== ruleCharacter)
    $ unPassword rowPassword

-- | Returns whether the given 'DatabaseRow' is valid according to the new policy.
--
-- >>> isValidNewDatabaseRow (DatabaseRow (PasswordRule 'a' 1 3) (Password "abcde"))
-- True
--
-- >>> isValidNewDatabaseRow (DatabaseRow (PasswordRule 'b' 1 3) (Password "cdefg"))
-- False
isValidNewDatabaseRow :: DatabaseRow -> Bool
isValidNewDatabaseRow DatabaseRow { rowRule = PasswordRule {..}, rowPassword }
    = uncurry xor
    $ bimapBoth (== Just ruleCharacter)
    $ (!!? (ruleMinCount - 1)) &&& (!!? (ruleMaxCount - 1))
    $ unPassword rowPassword


{- $puzzle1
== --- Day 2: Password Philosophy ---

Your flight departs in a few days from the coastal airport; the easiest way down to the
coast from here is via <https://en.wikipedia.org/wiki/Toboggan toboggan>.

The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day. \"Something\'s
wrong with our computers; we can\'t log in!\" You ask if you can take a look.

Their password database seems to be a little corrupted: some of the passwords wouldn\'t
have been allowed by the Official Toboggan Corporate Policy that was in effect when they
were chosen.

To try to debug the problem, they have created a list (your puzzle input) of /passwords/
(according to the corrupted database) and /the corporate policy when that password was/
/set/.

For example, suppose you have the following list:

> 1-3 a: abcde
> 1-3 b: cdefg
> 2-9 c: ccccccccc

Each line gives the password policy and then the password. The password policy indicates
the lowest and highest number of times a given letter must appear for the password to be
valid. For example, @1-3 a@ means that the password must contain @a@ at least @1@ time
and at most @3@ times.

In the above example, @2@ passwords are valid. The middle password, @cdefg@, is not; it
contains no instances of @b@, but needs at least @1@. The first and third passwords are
valid: they contain one @a@ or nine @c@, both within the limits of their respective
policies.

/How many passwords are valid/ according to their policies?
-}

{- $puzzle2
== --- Part Two ---
#part2#

While it appears you validated the passwords correctly, they don\'t seem to be what the
Official Toboggan Corporate Authentication System is expecting.

The shopkeeper suddenly realizes that he just accidentally explained the password policy
rules from his old job at the sled rental place down the street! The Official Toboggan
Corporate Policy actually works a little differently.

Each policy actually describes two /positions in the password/, where @1@ means the first
character, @2@ means the second character, and so on. (Be careful; Toboggan Corporate
Policies have no concept of \"index zero\"!) /Exactly one of these positions/ must
contain the given letter. Other occurrences of the letter are irrelevant for the purposes
of policy enforcement.

Given the same example list from above:

-   @1-3 a: abcde@ is /valid/: position @1@ contains @a@ and position @3@ does not.
-   @1-3 b: cdefg@ is /invalid/: neither position @1@ nor position @3@ contains @b@.
-   @2-9 c: ccccccccc@ is /invalid/: both position @2@ and position @9@ contain @c@.

/How many passwords are valid/ according to the new interpretation of the policies?
-}
