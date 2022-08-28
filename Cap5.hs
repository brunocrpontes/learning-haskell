module Cap5 where

-- 1
data TipoProduto = Escritorio | Informatica | Livro | Filme | Total

data Produto
  = Nada
  | Produto
      { valor :: Double,
        tipo :: TipoProduto
      }

instance Semigroup Produto where
  (<>) Nada produto = produto
  (<>) produto Nada = produto
  (<>) (Produto valor1 _) (Produto valor2 _) = Produto (valor1 + valor2) Total

instance Monoid Produto where
  mempty = Nada
  mappend = (<>)

-- 2
totalGeral :: [Produto] -> Double
totalGeral = valor . foldl (<>) Nada

-- 3
newtype Min = Min Int deriving (Ord, Eq, Show)

instance Semigroup Min where
  (<>) (Min value1) (Min value2)
    | value1 > value2 = Min value2
    | otherwise = Min value1

instance Monoid Min where
  mempty = Min maxBound
  mappend = (<>)

-- 4
minAll :: [Min] -> Min
minAll = foldl mappend mempty

-- 5
data Paridade = Par | Impar

class ParImpar a where
  decide :: a -> Paridade

instance ParImpar Int where
  decide value
    | even value = Par
    | otherwise = Impar

instance ParImpar [a] where
  decide list
    | even (length list) = Par
    | otherwise = Impar

instance ParImpar Bool where
  decide bool
    | bool = Impar
    | otherwise = Par

-- 6
newtype Max = Max Int deriving (Ord, Eq, Show)

instance Semigroup Max where
  (<>) (Max value1) (Max value2)
    | value1 > value2 = Max value1
    | otherwise = Max value2

instance Monoid Max where
  mempty = Max minBound
  mappend = (<>)

maxAll :: [Max] -> Max
maxAll = foldl mappend mempty

-- 7
data Arvore a
  = Vazio
  | Folha a
  | Galho a (Arvore a) (Arvore a)
  deriving (Show, Read)

emOrdem :: (a -> a) -> Arvore a -> Arvore a
emOrdem f arvore = arvore

data Coisa a = ZeroCoisa | UmaCoisa a | DuasCoisas a a deriving (Read, Show)
