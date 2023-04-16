-- 1) resposta é a questão c

-- 2)
-- a) Mostre a quantidade de vogais de uma String [Char]
-- "Papagaio" = 5 vogais

ehVogal :: Char -> Bool
ehVogal c = c `elem` "aeiouAEIOU"

quantidadeVogais :: [Char] -> Int
quantidadeVogais xs = length [c | c <- xs, ehVogal c]

-- b) Mostre a quantidade de positivos incluindo o zero de uma [Int]
-- [(-2), 3, 2, 5, (-4), (-15), 0]
ehPositivo :: Int -> Bool
ehPositivo n
  | n >= 0 = True
  | otherwise = False

quantidadePositivos :: [Int] -> Int
quantidadePositivos xs = length $ filter ehPositivo xs

-- c) Mostre a quantidade de caracteres ‘a‘ em uma String [Char]
-- "Papagaio" = 3

ehLetraA :: Char -> Bool
ehLetraA c = c `elem` "aA"

quantidadeLetrasA :: [Char] -> Int
quantidadeLetrasA xs = length [c | c <- xs, ehLetraA c]

-- d) Retorne a metade de todos os numeros de um [Double]
-- [1..10]
metadeNumeros :: [Double] -> [Double]
metadeNumeros = map (/ 2)

-- 3)
-- a) Crie o tipo de dado algebrico Cor que possui os value constructors Vermelho,Verde, Azul.
data Cor = Vermelho | Verde | Azul deriving (Show, Eq)

-- b) Crie um tipo de dado algebrico Construcao com os value constructor Casa. Uma casa possui cor, metragem,preco;
data Construcao = Casa {cor :: Cor, metragem :: Double, preco :: Double} deriving (Show)

-- c) Implemente a funcao casasVermelhas que recebe uma lista do tipo Casa e retorna apenas uma lista com casas da cor vermelha.
-- [(Casa Vermelho 10 9.99), (Casa Azul 10 9.99), (Casa Verde 10 9.99)]
casasVermelhas :: [Construcao] -> [Construcao]
casasVermelhas = filter (\(Casa cor metragem preco) -> cor == Vermelho)

-- d) Implemente a funcao totalCasasVermelhas que recebe uma lista do tipo Construcao e retorna o valor total(somado) de todas as casas vermelhas.
-- [(Casa Vermelho 10 9.99), (Casa Azul 10 9.99), (Casa Verde 10 9.99),(Casa Vermelho 10 9.99), (Casa Azul 10 9.99), (Casa Verde 10 9.99)]
quantidadeCasasVermelhas :: [Construcao] -> Int
quantidadeCasasVermelhas = length . casasVermelhas

precoTotalCasasVermelhas :: [Construcao] -> Double
precoTotalCasasVermelhas xs = foldl (\acc (Casa cor metragem preco) -> preco + acc) 0 $ casasVermelhas xs

-- 4) Implemente uma funcao que devolva o maior numero de uma [Int]. Deve ser implementada recursivamente.
-- [-2,3,-5,4,-1,1]
-- [-255..4]
maxInt :: [Int] -> Int
maxInt [] = 0
maxInt [x] = x
maxInt (x : y : ys)
  | x > y = maxInt (x : ys)
  | otherwise = maxInt (y : ys)

-- 5) Calcule as express ̃oes abaixo (mostre o resultado):
-- (a) :t map (\ x -> x + x * x) [3,4,5]
-- [Int]

-- (b) filter (\ x -> x /= 91) [90..100]
-- [90,92,93,94,95,96,97,98,99,100]

-- (c) :t (3,4)
-- (Int, Int)

-- (d) :fold (\ a b -> a - b) 0 [5..1]
-- ERRADO:
-- falta o "t" no :t
-- falta o "l" no foldl

-- (d) :t foldl (\ a b -> a - b) 0 [5..1]
-- Int