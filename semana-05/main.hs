-- 4.3) Implemente uma função que filtre os números pares e outra que filtre os ímpares de uma lista recebida via parâmetro.

retornaImpares :: [Int] -> [Int]
retornaImpares = filter odd

retornaPares :: [Int] -> [Int]
retornaPares = filter even

--------------------------------------------------------------------------------------------------------------------------------------------

-- 4.5) Implemente uma função que receba uma lista de inteiros e retorne o dobro de todos, eliminando os múltiplos de 4.
-- currying (*2)
-- map

-- $ (tudo a direita executa primeiro)
-- filter
-- lambda

retornaDobroDosNaoMultiplosDeQuatro :: [Int] -> [Int]
retornaDobroDosNaoMultiplosDeQuatro xs = map (* 2) $ filter (\x -> x `mod` 4 /= 0) xs

--------------------------------------------------------------------------------------------------------------------------------------------

-- 4.6) Faça uma função func que receba uma função f de tipo (String -> String) , e uma String s que retorna o reverso
-- de s concatenado com aplicação da função f em s .

-- reverter s
-- aplicar "f" em s

func :: (String -> String) -> String -> String
func f = f . reverse

-- func f s = f . reverse $ s
-- primeira_funcao . segunda_funcao

--------------------------------------------------------------------------------------------------------------------------------------------

-- 4.8) Implemente o tipo Dinheiro que contenha os campos valor e correncia ( Real ou Dolar ), e uma função que converta todos
-- os "dinheiros" de uma lista para dólar (e outra para real). Com isso, implemente funções para:

-- Libras só para teste (desconsiderar)
data Correncia = Real | Dolar | Libras deriving (Show, Eq)

data Dinheiro = Dinheiro {valor :: Double, correncia :: Correncia} | Null deriving (Show)

-- ***** FORMA GERAL *****

-- pattern matching
converteDinheiro :: Dinheiro -> Dinheiro
converteDinheiro (Dinheiro valor Real) = Dinheiro (valor * 5.20) Dolar
converteDinheiro (Dinheiro valor Dolar) = Dinheiro (valor / 5.20) Real

-- guard
converteDinheiroComGuard :: Dinheiro -> Dinheiro
converteDinheiroComGuard (Dinheiro valor correncia)
  | correncia == Real = Dinheiro (valor * 5.20) Dolar
  | correncia == Dolar = Dinheiro (valor / 5.20) Real

converteListaDeDinheiros :: [Dinheiro] -> [Dinheiro]
converteListaDeDinheiros = map converteDinheiro

-- ***** SÓ DOLARES *****

-- pattern matching
converteParaDolares :: Dinheiro -> Dinheiro
converteParaDolares (Dinheiro valor Real) = Dinheiro (valor * 5.20) Dolar
converteParaDolares (Dinheiro valor Dolar) = Dinheiro valor Dolar
converteParaDolares _ = Null -- otherwise

-- guard
converteParaDolaresComGuard :: Dinheiro -> Dinheiro
converteParaDolaresComGuard (Dinheiro valor correncia)
  | correncia == Real = Dinheiro (valor * 5.20) Dolar
  | correncia == Dolar = Dinheiro valor correncia
  | otherwise = Null

converteListaDeDinheirosParaDolares :: [Dinheiro] -> [Dinheiro]
converteListaDeDinheirosParaDolares = map converteParaDolares

-- ***** SÓ REAIS *****

-- guard
converteParaReaisComGuard :: Dinheiro -> Dinheiro
converteParaReaisComGuard (Dinheiro valor correncia)
  | correncia == Real = Dinheiro valor Real
  | correncia == Dolar = Dinheiro (valor / 5.20) Real
  | otherwise = Null

converteListaDeDinheirosParaReais :: [Dinheiro] -> [Dinheiro]
converteListaDeDinheirosParaReais = map converteParaReaisComGuard

------- 4.8) a) Filtrar todos os Dolares de uma lista de Dinheiro .
-- [(Dinheiro 1 Real), Null, (Dinheiro 10 Dolar), (Dinheiro 10 Libras)]

verificaDolares :: Dinheiro -> Bool
verificaDolares (Dinheiro valor correncia)
  | correncia == Dolar = True
  | otherwise = False
verificaDolares _ = False

filtraDolares :: [Dinheiro] -> [Dinheiro]
filtraDolares = filter verificaDolares

------- 4.8) b) Somar todos os Dolares de uma lista.
-- (b -> a -> b) -> b -> t a -> b
-- (acc -> param -> acc) -> valorInicialDoAcc -> [a] -> acc
somarDolares :: [Dinheiro] -> Double
somarDolares xs = foldl (\acc (Dinheiro valor correncia) -> acc + valor) 0 $ filtraDolares xs

------- 4.8) c) Contar a quantidade de Dolares de uma lista.
contarDolares :: [Dinheiro] -> Int
contarDolares xs = foldl (\acc _ -> acc + 1) 0 $ filtraDolares xs

contarDolaresSimp :: [Dinheiro] -> Int
contarDolaresSimp = length . filtraDolares

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) :: length -> filtraDolares -> xs -> c