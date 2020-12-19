{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE NoStarIsType              #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-|
Module      : Days
Description : Global types and functions for each day of the AoC calendar.
Copyright   : (c) blmage 2020
License     : MIT
-}
module Days
    (
      -- * Types
      Day(..)
    , Puzzle(..)
    , SDay(..)

     -- * Solutions
    , Solved(..)

      -- * Input
    , withInput
    , withInputLineStream
    , withInputLineList
    , withParsedInputLineStream
    , withParsedInputLineList
    , withGroupedParsedInputLineStream
    , withGroupedParsedInputLineList

      -- * Error handling
    , invalid
    , maybeToError

      -- * Parsing
    , Parser
    , runParser
    ) where

import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT, MonadError (..), liftEither, runExceptT)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.Char (isSpace)
import Data.Singletons.TH
import Data.Void (Void)
import Streaming
import System.IO (FilePath, IOMode (ReadMode), withFile)
import Text.Megaparsec (parse)
import Text.Megaparsec.Parsers (ParsecT (..))
import Text.Parser.Char (CharParsing)
import Text.Parser.Token (TokenParsing)

import qualified Streaming.Prelude as S
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Error as MP


-- | A day from the AoC calendar.
$(singletons [d|
    data Day
        = Day1
        | Day2
        | Day3
        | Day4
        | Day5
        | Day6
        | Day7
        | Day8
        | Day9
        | Day10
        | Day11
        | Day12
        | Day13
        | Day14
        | Day15
        | Day16
        | Day17
        | Day18
        | Day19
        | Day20
        | Day21
        | Day22
        | Day23
        | Day24
        | Day25
        deriving (Bounded, Enum, Eq, Ord, Show)
    |])


-- | A puzzle.
data Puzzle
    = Puzzle1
    | Puzzle2
    deriving (Bounded, Enum, Eq, Ord, Show)


-- | A `Day` whose `Puzzle`s have been solved.
class Solved (day :: Day) where
    -- | Returns the printable solution to a given `Puzzle`.
    solution :: Puzzle -> IO (Either String String)


-- | The 'FilePath' of the input of a 'Day'.
inputPath :: Day -> FilePath
inputPath day = "inputs/day" <> show (fromEnum day + 1) <> ".txt"


-- | Applies a function to the input corresponding to the given 'Day'.
withInputM
    :: forall m e a
     . MonadBaseControl IO m
    => Day
    -> (String -> ExceptT e m a)
    -> m (Either e a)
withInputM day f
    = runExceptT
    $ control
    $ \runInBase -> readFile (inputPath day) >>= runInBase . f

-- | Applies a function to the input corresponding to the given 'Day'.
withInput
    :: forall m e a
     . MonadBaseControl IO m
    => Day
    -> (forall n. MonadError e n => String -> n a)
    -> m (Either e a)
withInput = withInputM

-- | Applies a function to a 'Stream' of the input lines corresponding to the given 'Day'.
withInputLineStreamM
    :: forall m e a
     . ( MonadBaseControl IO m
       , MonadIO m
       )
    => Day
    -> (Stream (Of String) (ExceptT e m) () -> ExceptT e m a)
    -> m (Either e a)
withInputLineStreamM day f
    = runExceptT
    $ control
    $ \runInBase -> withFile (inputPath day) ReadMode $ runInBase . f . S.fromHandle

-- | Applies a function to a 'Stream' of the input lines corresponding to the given 'Day'.
withInputLineStream
    :: forall m e a
     . ( MonadBaseControl IO m
       , MonadIO m
       )
    => Day
    -> (forall n. MonadError e n => Stream (Of String) n () -> n a)
    -> m (Either e a)
withInputLineStream = withInputLineStreamM

-- | Applies a function to the list of the input lines corresponding to the given 'Day'.
withInputLineList
    :: forall m e a
     . ( MonadBaseControl IO m
       , MonadIO m
       )
    => Day
    -> (forall n. MonadError e n => [String] -> n a)
    -> m (Either e a)
withInputLineList day f
    = withInputLineStream day
    $ f
    . S.fst' <=< S.toList

-- | A parser for an input line.
type Parser a
     = forall s
     . ( MP.Stream s
       , CharParsing  (ParsecT Void s Identity)
       , TokenParsing (ParsecT Void s Identity)
       )
    => ParsecT Void s Identity a

-- | Runs a 'Parser' against a single, indexed line of input.
parseLine :: MonadError String n => Parser a -> (String, Int) -> n a
parseLine parser (line, index) = runParser parser ("Line " <> show index) line

-- | Applies a function to a 'Stream' of the input lines corresponding to the given 'Day',
-- parsed using the given 'Parser'.
withParsedInputLineStream
    :: forall m e a b
     . ( MonadBaseControl IO m
       , MonadIO m
       )
    => Day
    -> Parser a
    -> (forall n. (MonadIO n, MonadError String n) => Stream (Of a) n () -> n b)
    -> m (Either String b)
withParsedInputLineStream day parser f
    = withInputLineStreamM day
    $ f
    . S.mapM (parseLine parser)
    . S.scanned (const . (+ 1)) 0 id

-- | Applies a function to the list of the input lines corresponding to the given 'Day',
-- parsed using the given 'Parser'.
withParsedInputLineList
    :: forall m e a b
     . ( MonadBaseControl IO m
       , MonadIO m
       )
    => Day
    -> Parser a
    -> (forall n. (MonadIO n, MonadError String n) => [a] -> n b)
    -> m (Either String b)
withParsedInputLineList day parser f
      = withParsedInputLineStream day parser
      $ f
    <=< S.toList_

-- | Applies a function to a 'Stream' of the groups of input lines corresponding to the
-- given 'Day' that are separated by blank lines.
--
-- As with 'withParsedInputLineStream', each line is parsed using the given 'Parser'.
withGroupedParsedInputLineStream
    :: forall m e a b
     . ( MonadBaseControl IO m
       , MonadIO m
       )
    => Day
    -> Parser a
    -> (forall n. (MonadIO n, MonadError String n) => Stream (Stream (Of a) n) n () -> n b)
    -> m (Either String b)
withGroupedParsedInputLineStream day parser f
    = withInputLineStreamM day
    $ f
    . S.maps (S.mapM $ parseLine parser)
    . S.breaks (all isSpace . fst)
    . S.scanned (const . (+ 1)) 0 id

-- | Applies a function to a list of the groups of input lines corresponding to the given
-- 'Day' that are separated by blank lines.
--
-- As with 'withParsedInputLineSList', each line is parsed using the given 'Parser'.
withGroupedParsedInputLineList
    :: forall m e a b
     . ( MonadBaseControl IO m
       , MonadIO m
       )
    => Day
    -> Parser a
    -> (forall n. (MonadIO n, MonadError String n) => [NonEmpty a] -> n b)
    -> m (Either String b)
withGroupedParsedInputLineList day parser f
      = withGroupedParsedInputLineStream day parser
      $ (f . catMaybes . fmap nonEmpty)
    <=< S.toList_
      . S.mapped S.toList


-- | Runs a 'Parser' in a 'MonadError' context.
--
-- In case of failure, provides a precise error message.
runParser
    :: ( MP.Stream s
       , CharParsing  (ParsecT Void s Identity)
       , TokenParsing (ParsecT Void s Identity)
       , MonadError String n
       )
    => Parser a
    -> String
    -> s
    -> n a
runParser parser label
    = liftEither
    . first MP.errorBundlePretty
    . parse (unParsecT parser) label


-- | Returns an "Invalid [kind]: [value]." error message.
invalid :: Show a => String -> a -> String
invalid kind value = "Invalid " <> kind <> ": " <> show value <> "."

-- | Variant of 'maybeToRight' for 'MonadError'.
maybeToError :: MonadError e m => e -> Maybe a -> m a
maybeToError = (liftEither .) . maybeToRight
