module Cap7 where

import Cap5 (Arvore (..), Coisa (..))

-- 1
instance Functor Coisa where
  fmap _ ZeroCoisa = ZeroCoisa
  fmap f (UmaCoisa coisa) = UmaCoisa (f coisa)
  fmap f (DuasCoisas coisa1 coisa2) = DuasCoisas (f coisa1) (f coisa2)

-- 2
instance Applicative Coisa where
  pure = UmaCoisa
  (<*>) ZeroCoisa _ = ZeroCoisa
  (<*>) _ ZeroCoisa = ZeroCoisa
  (<*>) (UmaCoisa f) (UmaCoisa a) = UmaCoisa (f a)
  (<*>) (UmaCoisa f) (DuasCoisas a b) = DuasCoisas (f a) (f b)
  (<*>) (DuasCoisas f1 f2) (UmaCoisa a) = DuasCoisas (f1 a) (f2 a)
  (<*>) (DuasCoisas f1 f2) (DuasCoisas a b) = DuasCoisas (f1 a) (f2 b)

-- 3
mult234 :: Double -> Coisa Double
mult234 value = UmaCoisa (4 *) <*> (UmaCoisa (3 *) <*> (UmaCoisa (2 *) <*> UmaCoisa value))

instance Functor Arvore where
  fmap _ Vazio = Vazio
  fmap f (Folha a) = Folha (f a)
  fmap f (Galho a b c) = Galho (f a) (fmap f b) (fmap f c)

instance Applicative Arvore where
  pure = Folha
  (<*>) _ Vazio = Vazio
  (<*>) Vazio _ = Vazio
  (<*>) (Folha f) (Folha a) = Folha (f a)
  (<*>) (Folha f) (Galho a b c) = Galho (f a) (fmap f b) (fmap f c)
  (<*>) (Galho fa fr fl) (Folha a) = Folha (fa a)
  (<*>) (Galho fa fb fc) (Galho a b c) = Galho (fa a) (fb <*> b) (fc <*> c)

-- 4
data Fantasma a = Fantasma

-- 5
instance Functor Fantasma where
  fmap f Fantasma = Fantasma

-- 6
data Dupla a = Dupla a Int a

instance Functor Dupla where
  fmap f (Dupla a b c) = Dupla (f a) b (f c)

-- 7
newtype Derp a = Derp {runDerp :: Bool -> a}

instance Functor Derp where
  fmap f derp = Derp (f . runDerp derp)

instance Applicative Derp where
  pure a = Derp (const a)
  (<*>) (Derp runDerp1) (Derp runDerp2) = Derp (\x -> runDerp1 x (runDerp2 x))
