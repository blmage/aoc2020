{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Day07
Description : Handy Haversacks
Copyright   : (c) blmage 2020
License     : MIT
-}
module Day07
       (
         -- * Puzzle 1
         -- $puzzle1
         countBagsContaining

         -- * Puzzle 2
         -- $puzzle2
       , bagSize

         -- * Global types and functions
       , Bag
       , BagContent
       , Color
       , Qualifier
       , shinyGoldBag
       , bagParser
       , bagContentParser
       , ruleParser
       , colorParser
       , qualifierParser
       , withRules
       ) where

import Control.Applicative.Combinators (someTill)
import Control.Monad.Except (liftEither)
import Data.Interned.Text (InternedText)
import Data.Map.Strict (Map)
import Data.String.Interpolate (i)
import Text.Parser.Char (anyChar, char, space, string)
import Text.Parser.Combinators (manyTill, sepBy1, skipSome)
import Text.Parser.Token (decimal)

import Days

import qualified Data.Map.Strict as M
import qualified Data.Set as S


instance Solved Day7 where
    solution puzzle
        = withParsedInputLineList Day7 ruleParser
        $ liftEither
        . fmap show
        . go shinyGoldBag
        . M.fromList
      where
        go :: Bag -> Map Bag BagContent -> Either String Integer
        go = case puzzle of
            Puzzle1 -> countBagsContaining
            Puzzle2 -> bagSize


-- | A color.
newtype Color = Color { unColor :: InternedText }
    deriving newtype (Eq, IsString, Ord, Show)

-- | A qualifier for a 'Color'.
newtype Qualifier = Qualifier { unQualifier :: InternedText }
    deriving newtype (Eq, IsString, Ord, Show)

-- | The content of a 'Bag'.
newtype BagContent = BagContent { unBagContent :: Map Bag Integer }
    deriving newtype (Monoid, Semigroup, Show)

-- | A bag.
data Bag
    = Bag
    { -- | The 'Color' of the 'Bag'.
      bagColor     :: !Color
    , -- | The 'Qualifier' for the 'Color' of the 'Bag'.
      bagQualifier :: !Qualifier
    }
    deriving (Eq, Ord, Show)


-- | A 'Parser' for a rule specifying the 'BagContent' of a 'Bag'.
ruleParser :: Parser (Bag, BagContent)
ruleParser = do
    bag     <- bagParser
    string "bags contain"
    skipSome space
    content <- bagContentParser
    pure (bag, content)

-- | A 'Parser' for a 'Bag'.
bagParser :: Parser Bag
bagParser = do
    bagQualifier <- qualifierParser
    bagColor     <- colorParser
    pure $ Bag {..}

-- | A 'Parser' for a 'Color'.
colorParser :: Parser Color
colorParser = fromString <$> someTill anyChar space

-- | A 'Parser' for the 'Qualifier' of a 'Color'.
qualifierParser :: Parser Qualifier
qualifierParser = fromString <$> someTill anyChar space

-- | A 'Parser' for the 'BagContent' of a 'Bag'.
bagContentParser :: Parser BagContent
bagContentParser = do
    content <- emptyContentParser <|> someContentParser
    char '.'
    pure $ BagContent $ fromList content
  where
    emptyContentParser :: Parser [(Bag, Integer)]
    emptyContentParser = [] <$ string "no other bags"

    someContentParser :: Parser [(Bag, Integer)]
    someContentParser = bagQtyParser `sepBy1` (char ',' *> skipSome space)

    bagQtyParser :: Parser (Bag, Integer)
    bagQtyParser = do
        count <- decimal
        skipSome space
        bag   <- bagParser
        string $ if count > 1 then "bags" else "bag"
        pure (bag, count)


-- | A shiny gold 'Bag'.
shinyGoldBag :: Bag
shinyGoldBag = Bag "gold" "shiny"


-- | The state of a recursive search inside the content of one or more 'Bag's.
data SearchState a
    = SearchState
    { -- | The stack of 'Bag's inside which we are currently searching.
      searchBagStack    :: ![Bag]
    , -- | The cache of search results which have already been computed.
      searchResultCache :: !(Map Bag a)
    }

-- | A function helping with recursively searching inside the content of one or more
-- 'Bag's, by taking care of:
--
-- * fetching the 'BagContent' for some given 'Bag',
-- * caching the intermediate results,
-- * detecting and short-circuiting infinite loops.
withRules
    :: forall a b
     . (forall m. Monad m => (Bag -> m a) -> BagContent -> m a)
    -- ^ A function returning the result for a single 'BagContent'.
    -> (forall m. Monad m => (Bag -> m a) -> m b)
    -- ^ A function returning the global result.
    -> Map Bag BagContent
    -- ^ A set of rules specifying the 'BagContent's of each 'Bag'.
    -> Either String b
withRules g f rules = evalStateT (f bagResult) (SearchState mempty mempty)
  where
    bagResult :: Bag -> StateT (SearchState a) (Either String) a
    bagResult bag = do
        state@SearchState {..} <- get

        let isLooping = bag `elem` searchBagStack
        let loop = reverse $ bag : searchBagStack
        when isLooping $ fail $ [i|Infinite loop detected: #{loop}.|]

        whenNothing (M.lookup bag searchResultCache) $ do
            put $ state { searchBagStack = bag : searchBagStack }
            result <- g bagResult $ fold $ M.lookup bag rules
            put $ state { searchResultCache = M.insert bag result searchResultCache }
            pure result


-- | Count the number of different 'Bag's containing the given 'Bag' according to the
-- given set of rules.
countBagsContaining :: Bag -> Map Bag BagContent -> Either String Integer
countBagsContaining searched rules
      = fromIntegral
      . length
    <$> withRules hasSearched findAll rules
  where
    hasSearched :: Monad m => (Bag -> m Any) -> BagContent -> m Any
    hasSearched go (unBagContent -> content)
        | searched `M.member` content
        = pure $ Any True
        | otherwise
        = foldMapM go $ M.keys content

    findAll :: Monad m => (Bag -> m Any) -> m [Bag]
    findAll go = filterM (fmap getAny . go) $ M.keys rules

-- | Returns the size of the given 'Bag' according to the given set of rules.
bagSize :: Bag -> Map Bag BagContent -> Either String Integer
bagSize bag rules = withRules subSize ((subtract 1 <$>) . ($ bag)) rules
  where
    subSize :: Monad m => (Bag -> m Integer) -> BagContent -> m Integer
    subSize go
        = fmap ((+ 1) . sum)
        . traverse ((uncurry (*) <$>) . traverse go . swap)
        . M.toList
        . unBagContent


{- $puzzle1
== --- Day 7: Handy Haversacks ---

You land at the regional airport in time for your next flight. In fact, it looks like
you\'ll even have time to grab some food: all flights are currently delayed due to
/issues in luggage processing/.

Due to recent aviation regulations, many rules (your puzzle input) are being enforced
about bags and their contents; bags must be color-coded and must contain specific
quantities of other color-coded bags. Apparently, nobody responsible for these
regulations considered how long they would take to enforce!

For example, consider the following rules:

> light red bags contain 1 bright white bag, 2 muted yellow bags.
> dark orange bags contain 3 bright white bags, 4 muted yellow bags.
> bright white bags contain 1 shiny gold bag.
> muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
> shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
> dark olive bags contain 3 faded blue bags, 4 dotted black bags.
> vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
> faded blue bags contain no other bags.
> dotted black bags contain no other bags.

These rules specify the required contents for 9 bag types. In this example, every
@faded blue@ bag is empty, every @vibrant plum@ bag contains 11 bags (5 @faded blue@ and
6 @dotted black@), and so on.

You have a @shiny gold@ bag. If you wanted to carry it in at least one other bag, how
many different bag colors would be valid for the outermost bag? (In other words: how many
colors can, eventually, contain at least one @shiny gold@ bag?)

In the above rules, the following options would be available to you:

-   A @bright white@ bag, which can hold your @shiny gold@ bag directly.
-   A @muted yellow@ bag, which can hold your @shiny gold@ bag directly, plus some other
    bags.
-   A @dark orange@ bag, which can hold @bright white@ and @muted yellow@ bags, either of
    which could then hold your @shiny gold@ bag.
-   A @light red@ bag, which can hold @bright white@ and @muted yellow@ bags, either of
    which could then hold your @shiny gold@ bag.

So, in this example, the number of bag colors that can eventually contain at least one
@shiny gold@ bag is @4@.

/How many bag colors can eventually contain at least one @shiny gold@ bag?/ (The list of
rules is quite long; make sure you get all of it.)
-}

{- $puzzle2
== --- Part Two ---
#part2#

It\'s getting pretty expensive to fly these days - not because of ticket prices, but
because of the ridiculous number of bags you need to buy!

Consider again your @shiny gold@ bag and the rules from the above example:

-   @faded blue@ bags contain @0@ other bags.
-   @dotted black@ bags contain @0@ other bags.
-   @vibrant plum@ bags contain @11@ other bags: 5 @faded blue@ bags and 6 @dotted black@
    bags.
-   @dark olive@ bags contain @7@ other bags: 3 @faded blue@ bags and 4 @dotted black@
    bags.

So, a single @shiny gold@ bag must contain 1 @dark olive@ bag (and the 7 bags within it)
plus 2 @vibrant plum@ bags (and the 11 bags within /each/ of those): @1 + 1*7 + 2 + 2*11@
= @32@ bags!

Of course, the actual rules have a small chance of going several levels deeper than this
example; be sure to count all of the bags, even if the nesting becomes topologically
impractical!

Here\'s another example:

> shiny gold bags contain 2 dark red bags.
> dark red bags contain 2 dark orange bags.
> dark orange bags contain 2 dark yellow bags.
> dark yellow bags contain 2 dark green bags.
> dark green bags contain 2 dark blue bags.
> dark blue bags contain 2 dark violet bags.
> dark violet bags contain no other bags.

In this example, a single @shiny gold@ bag must contain @126@ other bags.

/How many individual bags are required inside your single @shiny gold@ bag?/
-}
