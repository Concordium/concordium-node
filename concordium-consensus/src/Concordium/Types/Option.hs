{-# LANGUAGE DeriveTraversable #-}

-- | This module provides a strict version of 'Maybe'.
module Concordium.Types.Option (
    -- * Strict version of 'Maybe'.
    Option (..),

    -- * Auxiliary functions
    putOptionOf,
    getOptionOf,
    isPresent,
    isAbsent,
    fromOption,
    ofOption,
) where

import Data.Serialize

-- | A strict version of 'Maybe'.
data Option a
    = Absent
    | Present !a
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Putter for an @Option a@.
putOptionOf :: Putter a -> Putter (Option a)
putOptionOf _ Absent = putWord8 0
putOptionOf pa (Present a) = putWord8 1 >> pa a

-- | Getter for an @Option a@.
getOptionOf :: Get a -> Get (Option a)
getOptionOf ma = do
    getWord8 >>= \case
        0 -> return Absent
        1 -> Present <$> ma
        _ -> fail "invalid tag for Option"

-- | 'Serialize' instance for an @Option a@.
instance (Serialize a) => Serialize (Option a) where
    put = putOptionOf put
    get = getOptionOf get

-- | Returns 'True' if and only if the value is 'Present'.
isPresent :: Option a -> Bool
isPresent Absent = False
isPresent (Present _) = True

-- | Returns 'True' if and only if the value is 'Absent'.
isAbsent :: Option a -> Bool
isAbsent Absent = True
isAbsent (Present _) = False

-- | Get the contents of an 'Option' or the supplied default value if it is 'Absent'.
fromOption :: a -> Option a -> a
fromOption def Absent = def
fromOption _ (Present v) = v

-- | Deconstruct an 'Option', returning the first argument if it is 'Absent', and otherwise
--  applying the second argument to the value if it is 'Present'. (Analogous to 'maybe'.)
ofOption :: b -> (a -> b) -> Option a -> b
ofOption ab _ Absent = ab
ofOption _ pr (Present v) = pr v
