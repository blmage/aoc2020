{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Constraint (Dict (..))
import Data.List (intercalate)
import Data.Singletons (withSomeSing)
import GHC.Exts (IsList (..))

import Day01 ()
import Days


main :: IO ()
main = loop
  where
    loop :: IO ()
    loop = putStr "Enter a day ([1-25] / 'q' to quit): " *> getLine >>= \case
        "q" -> putStrLn "Quitting."
        n   -> handleDay (toString n) *> loop

    handleDay :: String -> IO ()
    handleDay n = case readMaybe @Int n >>= safeToEnum @Day . subtract 1 of
        Nothing  -> putStrLn $ "Please enter a number between 1 and 25."
        Just day -> withSomeSing day daySolutions >>= \case
            Nothing        -> putStrLn $ "Day #" <> n <> " has not yet been solved."
            Just solutions -> do
                putStrLn $ "Day #" <> n <> " - Solutions:"
                forM_ (zip [1..] solutions) $ \(n, solution) ->
                    putStrLn
                         $ "Puzzle #"
                        <> show n
                        <> ": "
                        <> either ("*error* " <>) ("\n" <>) solution


-- | Returns 'Just' the 'Solved' 'Dict' for a 'Solved' 'Day', otherwise 'Nothing'.
solvedDict :: forall (day :: Day). SDay day -> Maybe (Dict (Solved day))
solvedDict = \case
    SDay1 -> Just Dict
    _     -> Nothing

-- | Returns 'Just' the printable solutions for a 'Solved' 'Day', otherwise 'Nothing'.
daySolutions :: forall (day :: Day) a. SDay day -> IO (Maybe [Either String String])
daySolutions day = case solvedDict day of
    Nothing   -> pure Nothing
    Just Dict -> Just <$> traverse (solution @day) (enumFrom Puzzle1)
