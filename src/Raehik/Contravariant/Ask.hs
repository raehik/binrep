-- | A funky contravariant functor I found which has its uses in binrep.

module Raehik.Contravariant.Ask where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void ( absurd )

{- | Obtain an @m@ either by mapping on @a@, or simply unwrapping a plain @m@.

This type explicitly encodes the pattern where we have an operation on a type
@a@, but sometimes we may not need to look at the argument to know the answer
-- it's constant, independent of the argument's value. Usually, you do this
by ignoring the argument with @_@ or @const@. Here, you may write 'Ignore' and
never even accept an argument. Of course, you need to provide an @a@ to extract
@m@, but this explicit "no argument thank you" might enable better combination
behaviour. (I actually have no idea.)

This is a contravariant functor. They are weird. You may combine them using
operations over 'Contravariant's. They are funky. Hoogle 'Divisible'.

@m@ should be a 'Monoid'.

I don't know if it's useful, but this type is equivalent to:

* 'Op' with an extra constructor.
* @'Either' (a -> m) m@.
* @Size a@ type in the store library, when @m ~ 'Sum' 'Int'@

TODO: unsure on type/cstr name. other ideas: need/known?
-}
data Ask m a
  -- | @m@ depends on @a@.
  = Use (a -> m)

  -- | @m@ is independent of @a@.
  --
  -- TODO store used ! here
  | Ignore m

instance Contravariant (Ask m) where
    contramap f = \case
      Ignore m -> Ignore      m
      Use    g -> Use $ \a -> g (f a)

instance Monoid m => Divisible (Ask m) where
    conquer = Ignore mempty
    divide toLR ls rs = case (ls, rs) of
      (Ignore lc, Ignore rc) -> Ignore $ lc <> rc
      (Use    lv, Ignore rc) -> Use $ \a ->
        let (l, _) = toLR a
        in  lv l <> rc
      (Ignore lc, Use    rv) -> Use $ \a ->
        let (_, r) = toLR a
        in  lc   <> rv r
      (Use    lv, Use    rv) -> Use $ \a ->
        let (l, r) = toLR a
        in  lv l <> rv r

instance Monoid m => Decidable (Ask m) where
    lose f = Use $ \a -> absurd (f a)
    choose split ls rs = case (ls, rs) of
      (Ignore lc, Ignore rc) -> Ignore  $ lc <> rc
      (Use    lv, Ignore rc) -> Use $ \a ->
        case split a of
          Left  l -> lv l
          Right{} -> rc
      (Ignore lc, Use    rv) -> Use $ \a ->
        case split a of
          Left{}  -> lc
          Right r -> rv r
      (Use    lv, Use    rv) -> Use $ \a ->
        case split a of
          Left  l -> lv l
          Right r -> rv r

instance Semigroup m => Semigroup (Ask m a) where
    l <> r = case (l, r) of
      (Ignore lc, Ignore rc) -> Ignore $    lc   <> rc
      (Use    lv, Ignore rc) -> Use $ \a -> lv a <> rc
      (Ignore lc, Use    rv) -> Use $ \a -> lc   <> rv a
      (Use    lv, Use    rv) -> Use $ \a -> lv a <> rv a

instance Monoid m => Monoid (Ask m a) where mempty = Ignore mempty
