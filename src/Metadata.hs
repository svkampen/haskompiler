{-# LANGUAGE TypeOperators, FlexibleContexts, DefaultSignatures, RankNTypes, ExistentialQuantification, DuplicateRecordFields, InstanceSigs, DeriveFunctor, DeriveDataTypeable, FlexibleInstances #-}

{-
 - Data types and their instances for metadata such as source location.
 -}

module Metadata
    (
        SourcePos(..), WithPos(..), wpReplaceVal, SourceSpan(..), HasSpan(..), spanBetween, mkSourceSpan
    )
    where

import Text.Megaparsec (SourcePos(..), mkPos, unPos, initialPos)
import Text.Printf
import Data.Data
import GHC.Generics
import Control.Monad (void)

-- | A wrapper which adds positional information to a value (such as a `CivicToken`).
data WithPos a = WithPos 
    { startPos :: SourcePos,
      endPos :: SourcePos,
      startOffset :: !Int,
      value :: a
    } deriving (Eq, Ord, Functor, Data, Typeable)

identityWP :: WithPos ()
identityWP = WithPos initialPos initialPos invalidStart ()
    where initialPos = SourcePos "" (mkPos 1) (mkPos 1)
          invalidStart = -1

wpReplaceVal :: Functor f => f a -> b -> f b
wpReplaceVal wp x = x <$ wp

instance Semigroup (WithPos ()) where
    (<>) :: WithPos () -> WithPos () -> WithPos ()
    a <> b
      | a == identityWP = b
      | b == identityWP = a
      | otherwise = WithPos {
            startPos = min (startPos a) (startPos b),
            endPos   = max (endPos a) (endPos b),
            startOffset = min (startOffset a) (startOffset b),
            value = ()
        }

instance Monoid (WithPos ()) where
    mempty = identityWP
    mappend = (<>)

instance Applicative WithPos where
    pure = (<$ identityWP)
    f@WithPos{value=f'} <*> v@WithPos{value=v'} = f' v' <$ (void f <> void v)

instance Monad WithPos where
    return = pure
    wp@WithPos{value=v} >>= f = let wp'@WithPos{value=v'} = f v
                                 in v' <$ (void wp <> void wp')

instance Show a => Show (WithPos a) where
    show (WithPos (SourcePos inp ln col) (SourcePos _ ln' col') _ value) =
        show value ++ printf " at %d:%d-%d:%d of %s" (unPos ln) (unPos col) (unPos ln') (unPos col') inp

data SourceSpan = SourceSpan
    { ssStart  :: SourcePos
    , ssEnd    :: SourcePos
    , ssOffset :: !Int
    } deriving (Eq, Ord, Data, Typeable)

instance Semigroup SourceSpan where
    (SourceSpan s e o) <> (SourceSpan s' e' o') =
        SourceSpan {
            ssStart = min s s',
            ssEnd = max e e',
            ssOffset = min o o'
        }

mkSourceSpan :: FilePath -> (Int, Int) -> (Int, Int) -> Int -> SourceSpan
mkSourceSpan fp (l, c) (l', c') = SourceSpan startPos endPos
    where startPos = SourcePos fp (mkPos l) (mkPos c)
          endPos   = SourcePos fp (mkPos l') (mkPos c')

instance Show SourceSpan where
    show span = let SourcePos{sourceName=sN,sourceLine=sL,sourceColumn=sC} = ssStart span
                    SourcePos{sourceLine=sL',sourceColumn=sC'} = ssEnd span
                 in printf "%s:%d:%d-%d:%d" sN (unPos sL) (unPos sC) (unPos sL') (unPos sC')

class HasSpan a where
    getSpan :: a -> SourceSpan
    default getSpan :: (Generic a, GHasSpan (Rep a)) => a -> SourceSpan
    getSpan a = ggetSpan (from a)

instance HasSpan SourceSpan where
    getSpan = id

class GHasSpan f where
    ggetSpan :: f a -> SourceSpan

-- N.B. GHasSpan always picks rightmost element of product types
-- (so Foo SourceSpan bar will only have a GHasSpan instance if bar has a
--  GHasSpan instance). Suggested course of action: put your SourceSpan last :P
instance (GHasSpan b) => GHasSpan (a :*: b) where
    ggetSpan (_ :*: b) = ggetSpan b

instance (GHasSpan a) => GHasSpan (M1 i c a) where
    ggetSpan (M1 x) = ggetSpan x

instance (GHasSpan a, GHasSpan b) => GHasSpan (a :+: b) where
    ggetSpan (L1 x) = ggetSpan x
    ggetSpan (R1 x) = ggetSpan x

instance (HasSpan a) => GHasSpan (K1 i a) where
    ggetSpan (K1 x) = getSpan x

instance HasSpan () where
    getSpan _ = SourceSpan p p (-1)
        where p = initialPos ""

spanBetween :: forall a b. WithPos a -> WithPos b -> SourceSpan
spanBetween wp wp' = let mergedWP = void wp <> void wp' in SourceSpan
    {
         ssStart  = startPos mergedWP,
         ssEnd    = endPos mergedWP,
         ssOffset = startOffset mergedWP
    }
