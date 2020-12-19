{-# LANGUAGE DerivingStrategies #-}

{-|
Module      : Day06
Description : Custom Customs
Copyright   : (c) blmage 2020
License     : MIT
-}
module Day06
       (
         -- * Puzzle 1
         -- $puzzle1
         anyYes

         -- * Puzzle 2
         -- $puzzle2
       , allYes

         -- * Global types and functions
       , Form
       , Answer
       , formParser
       , answerParser
       ) where

import Data.Foldable (foldr1)
import Data.Set (Set)
import Text.Parser.Char (satisfyRange)
import Text.Parser.Combinators (some)

import Days

import qualified Data.Set as S


instance Solved Day6 where
    solution puzzle
        = withGroupedParsedInputLineList Day6 formParser
        $ pure
        . show
        . sum
        . fmap (length . go . toList)
      where
        go :: [Form] -> Set Answer
        go = case puzzle of
            Puzzle1 -> anyYes
            Puzzle2 -> allYes


-- | A "yes" answer to a question from a customs declaration 'Form'.
newtype Answer = Answer { unAnswer :: Char }
    deriving (Eq, Ord)

-- | A customs declaration form.
newtype Form = Form { unForm :: Set Answer }


-- | A parser for an 'Answer'.
answerParser :: Parser Answer
answerParser = Answer <$> satisfyRange 'a' 'z'

-- | A parser for a 'Form'.
formParser :: Parser Form
formParser = Form . fromList <$> some answerParser


-- | Returns the 'Set' of all different yes 'Answer's from a list of 'Form's.
anyYes :: [Form] -> Set Answer
anyYes = foldr (S.union . unForm) mempty

-- | Returns the 'Set' of common yes 'Answer's from a list of 'Form's.
allYes :: [Form] -> Set Answer
allYes = foldMap (uncurry $ foldr S.intersection) . uncons . fmap unForm


{- $puzzle1
== --- Day 6: Custom Customs ---

As your flight approaches the regional airport where you\'ll switch to a much larger
plane, <https://en.wikipedia.org/wiki/Customs_declaration customs declaration forms> are
distributed to the passengers.

The form asks a series of 26 yes-or-no questions marked @a@ through @z@. All you need to
do is identify the questions for which /anyone in your group/ answers \"yes\". Since your
group is just you, this doesn\'t take very long.

However, the person sitting next to you seems to be experiencing a language barrier and
asks if you can help. For each of the people in their group, you write down the questions
for which they answer \"yes\", one per line. For example:

> abcx
> abcy
> abcz

In this group, there are /@6@/ questions to which anyone answered \"yes\": @a@, @b@, @c@,
@x@, @y@, and @z@. (Duplicate answers to the same question don\'t count extra; each
question counts at most once.)

Another group asks for your help, then another, and eventually you\'ve collected answers
from every group on the plane (your puzzle input). Each group\'s answers are separated by
a blank line, and within each group, each person\'s answers are on a single line. For
example:

> abc
>
> a
> b
> c
>
> ab
> ac
>
> a
> a
> a
> a
>
> b

This list represents answers from five groups:

-   The first group contains one person who answered \"yes\" to /@3@/ questions: @a@,
    @b@, and @c@.
-   The second group contains three people; combined, they answered \"yes\" to /@3@/
    questions: @a@, @b@, and @c@.
-   The third group contains two people; combined, they answered \"yes\" to /@3@/
    questions: @a@, @b@, and @c@.
-   The fourth group contains four people; combined, they answered \"yes\" to only /@1@/
    question, @a@.
-   The last group contains one person who answered \"yes\" to only /@1@/ question, @b@.

In this example, the sum of these counts is @3 + 3 + 3 + 1 + 1@ = /@11@/.

For each group, count the number of questions to which anyone answered \"yes\". /What is/
/the sum of those counts?/
-}

{- $puzzle2
== --- Part Two ---
#part2#

As you finish the last group\'s customs declaration, you notice that you misread one word
in the instructions:

You don\'t need to identify the questions to which /anyone/ answered \"yes\"; you need to
identify the questions to which /everyone/ answered \"yes\"!

Using the same example as above:

> abc
>
> a
> b
> c
>
> ab
> ac
>
> a
> a
> a
> a
>
> b

This list represents answers from five groups:

-   In the first group, everyone (all 1 person) answered \"yes\" to /@3@/ questions: @a@,
    @b@, and @c@.
-   In the second group, there is /no/ question to which everyone answered \"yes\".
-   In the third group, everyone answered yes to only /@1@/ question, @a@. Since some
    people did not answer \"yes\" to @b@ or @c@, they don\'t count.
-   In the fourth group, everyone answered yes to only /@1@/ question, @a@.
-   In the fifth group, everyone (all 1 person) answered \"yes\" to /@1@/ question, @b@.

In this example, the sum of these counts is @3 + 0 + 1 + 1 + 1@ = /@6@/.

For each group, count the number of questions to which /everyone/ answered \"yes\".
/What is the sum of those counts?/
-}
