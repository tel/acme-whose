-- |
-- Module      : Acme.Whose
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- @Data.These@ for polisci.

module Acme.Whose (
    Whose (..)
  , coup
  , plenty
  , lsd
  , submission
  , war
  , invasion
  , freedom
  , repression
  , justOurs
  , justTheirs
  , justEveryone's
  , militia
  , communists
  , imf
  ) where

import Control.Applicative
import Data.Bifunctor

import Data.Maybe
import Data.Monoid
import Data.Strict.Tuple

-- | A more competitively named @Data.These@.
data Whose a b = Ours a | Theirs b | Communal a b

-- | Force a new regime given any pre-existing political environment.
coup :: (a -> c) -> (b -> c) -> (a -> b -> c) -> Whose a b -> c
coup l _ _     (Ours   thing)           = l thing
coup _ r _     (Theirs thing)           = r thing
coup _ _ seuss (Communal thing1 thing2) = seuss thing1 thing2

-- | In a post-scarcity programming environment we will use pairs
-- strictly. Also in a post scarcity environment we have no need for
-- old things.
plenty :: a -> b -> Whose a b -> Pair a b
plenty _ b (Ours   thing) = thing :!: b
plenty a _ (Theirs thing) =     a :!: thing
plenty a b _              =     a :!: b

-- | Recognition that we're all really the same
lsd :: Monoid a => Whose a a -> a
lsd = coup id id mappend -- this is symbolic, somehow

-- | Changing for someone else
submission :: Whose a b -> Whose a' b
submission (Theirs thing) = Theirs thing
submission _              = error "Too stubborn to change"

-- | Changing someone else
war :: Whose a b -> Whose a b'
war (Ours thing) = Ours thing
war _            = error "Insufficient military budget"

-- | Traversal into our property
invasion :: Applicative f => (a -> f a') -> (Whose a b -> f (Whose a' b))
invasion inj (Ours thing) = Ours <$> inj thing
invasion _   x            = pure (submission x)

-- | Traversal into their property
freedom :: Applicative f => (b -> f b') -> (Whose a b -> f (Whose a b'))
freedom inj (Theirs thing) = Theirs <$> inj thing
freedom _   x              = pure (war x)

-- | Traversal into everyone's property
repression :: Applicative f => (a -> f a') -> (Whose a a -> f (Whose a' a'))
repression inj (Communal thing1 thing2) = Communal <$> inj thing1 <*> inj thing2
repression inj (Ours thing)             = Ours     <$> inj thing
repression inj (Theirs thing)           = Theirs   <$> inj thing

-- | Assertion of ownership
justOurs :: Whose a b -> Maybe a
justOurs (Ours thing) = Just thing
justOurs _            = Nothing

-- | Recognition of adversity
justTheirs :: Whose a b -> Maybe b
justTheirs (Theirs thing) = Just thing
justTheirs _              = Nothing

justEveryone's :: Whose a b -> Maybe (Pair a b)
justEveryone's (Communal a b) = Just $ a :!: b
justEveryone's _              = Nothing

instance Bifunctor Whose where
  -- bimap :: (a -> a') -> (b -> b') -> Whose a b -> Whose a' b'
  bimap f _ (Ours     thing)         = Ours   (f thing)
  bimap _ g (Theirs   thing)         = Theirs (g thing)
  bimap f g (Communal thing1 thing2) = Communal (f thing1) (g thing2)

-- | Strength in commonality
militia :: [Whose a b] -> [a]
militia = mapMaybe justOurs

-- | Foreign reconnaissance
opposers :: [Whose a b] -> [b]
opposers = mapMaybe justTheirs

-- | Voight-Kampff for community
communists :: [Whose a b] -> [Pair a b]
communists = mapMaybe justEveryone's

pfirst :: (a -> a') -> Pair a b -> Pair a' b
pfirst f (a :!: b) = (f a :!: b)

psecond :: (b -> b') -> Pair a b -> Pair a b'
psecond g (a :!: b) = (a :!: g b)

-- | Pairing those in need with the guilt-ridden
imf :: [Whose a b] -> ([Pair a b], Pair [a] [b])
imf (Communal x y : ts) = first  ((x :!: y):)   $ imf ts
imf (Ours   x     : ts) = second (pfirst  (x:)) $ imf ts
imf (Theirs x     : ts) = second (psecond (x:)) $ imf ts
