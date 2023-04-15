import Data.Char qualified as Char

-- recursão
-- Tipos de dados
-- Record syntax
-- Pattern matching
-- Funções:
-- 	lambda
-- 	map
-- 	foldl
-- 	filter

-- Guards
-- Clausula where

-- 4.9) sando a função foldl , crie lambdas para:

-- a) Contar números negativos de uma lista de Int .
contaNegativos :: [Int] -> Int
contaNegativos xs = foldl (\acc i -> (if i < 0 then acc + 1 else acc)) 0 xs

-- (b -> a -> b) ===> (\acc i -> (if i < 0 then acc+1 else acc)  )
-- b ===> 0
-- t a ===> [Int] ===> xs

-- [19, -3, -6, 10, -2, 0, 1, -5, 3, -4]

verificaNegativos :: Int -> Bool
verificaNegativos i
  | i < 0 = True
  | otherwise = False

contaNegativos2 :: [Int] -> Int
contaNegativos2 xs = foldl (\acc i -> acc + 1) 0 $ filter verificaNegativos xs

contaNegativos3 :: [Int] -> Int
contaNegativos3 xs = length $ filter verificaNegativos xs

negativos :: Int -> Int -> Int
negativos acc i
  | i < 0 = acc + 1
  | otherwise = acc

contaNegativos4 :: [Int] -> Int
contaNegativos4 = foldl (\acc i -> negativos acc i) 0

-- b) Contar letras 'P' de uma String

-- "Papagaio"

letrasP :: Int -> Char -> Int
letrasP acc letra
  | Char.toUpper letra == 'P' = acc + 1
  | otherwise = acc

letrasPMaiusculo :: Int -> Char -> Int
letrasPMaiusculo acc letra
  | letra == 'P' = acc + 1
  | otherwise = acc

contaLetrasP :: String -> Int
contaLetrasP = foldl (\acc i -> letrasP acc i) 0

contaLetrasPMaiusculo :: String -> Int
contaLetrasPMaiusculo = foldl (\acc i -> letrasPMaiusculo acc i) 0

-- c) Para contar Sabados em uma lista de um [DiaSemana] .

-- [Sabado, Segunda , Terca , Quarta , Quinta , Sexta , Sabado , Domingo]

data DiaSemana = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo deriving (Show, Eq)

sabados :: Int -> DiaSemana -> Int
sabados acc Sabado = acc + 1
sabados acc _ = acc

contaSabados :: [DiaSemana] -> Int
contaSabados = foldl (\acc dia -> sabados acc dia) 0

contaSabados2 :: [DiaSemana] -> Int
contaSabados2 = foldl sabados 0

-- d) Para, a partir de uma lista de [DiaSemana] , retornar a soma dos dias.
-- Exemplo: [Segunda, Segunda, Quarta] deve retornar 5 . Use uma função auxiliar para converter DiaSemana para Int .

diaSemanaParaInt :: DiaSemana -> Int
diaSemanaParaInt Segunda = 1
diaSemanaParaInt Terca = 2
diaSemanaParaInt Quarta = 3
diaSemanaParaInt Quinta = 4
diaSemanaParaInt Sexta = 5
diaSemanaParaInt Sabado = 6
diaSemanaParaInt Domingo = 7

somaDiasSemana :: [DiaSemana] -> Int
somaDiasSemana = foldl (\acc dia -> diaSemanaParaInt dia + acc) 0

{-
    5.7)
        Usando a estrutura de árvore, monte uma função mapa , que jogue uma função passada por parâmetro para todos os
        elementos de uma árvore. Deixe explícito o tipo desta função.
-}
{-
  f -> função
  Branch
    x -> valor (50 / 30 / 90 ...)
  l -> left (esquerda)
    x
    l
    r
  r -> right (direita)
    x
    l
    r
-}
data Arvore a
  = Nulo
  | Leaf a
  | Branch a (Arvore a) (Arvore a)
  deriving (Show)

mapa :: (Arvore a -> Arvore a) -> Arvore a -> Arvore a
mapa f (Branch x l r) = f (Branch x (mapa f l) (mapa f r))
mapa f (Leaf x) = f (Leaf x)
mapa f Nulo = Nulo

-- mapa somar5 (Branch 50 (Branch 30 (Leaf 20) (Leaf 40)) (Branch 90 Nulo (Leaf 100)))
{-
  Branch 50
    l -> (Branch 30 (Leaf 20) (Leaf 40))
      Branch 30
        l -> (Leaf 20)
        r -> (Leaf 40)
    r -> (Branch 90 Nulo (Leaf 100))
      Branch 90 + 5
        l -> Nulo
        r -> (Leaf 100)
-}

{-
    5.8)
        Usando o exercício anterior, some 5 a cada elemento de uma árvore de inteiros.
-}

somar5 :: Num a => Arvore a -> Arvore a
somar5 (Branch x l r) = Branch (x + 5) l r
somar5 (Leaf x) = Leaf (x + 5)
somar5 Nulo = Nulo

-- mapa somar5 (Branch 50 (Branch 30 (Leaf 20) (Leaf 40)) (Branch 90 Nulo (Leaf 100)))
{-
  Branch 50 + 5
    l -> (Branch 30 (Leaf 20) (Leaf 40))
      Branch 30 + 5
        l -> (Leaf 20 + 5)
        r -> (Leaf 40 + 5)
    r -> (Branch 90 Nulo (Leaf 100))
      Branch 90 + 5
        l -> Nulo
        r -> (Leaf 100 + 5)
-}