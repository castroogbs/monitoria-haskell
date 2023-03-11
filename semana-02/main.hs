-- data NomeNovoTipo = ValueConstructor1 | ValueConstructor2
-- data NomeNovoTipo = ValueConstructor1 String Int | ValueConstructor2 String
-- data NomeNovoTipo2 = ValueConstructor1 { param1, param2 :: String } | ValueConstructor2 { param1 :: String, param2 :: Int }
-- ! high-order function -> pode ser passada como param ou retorna uma função

------------------------------------

-- 3.1) Crie o tipo Pergunta com os values constructors Sim ou Nao . 
-- Faça as funções seguintes, determinando seus tipos explicitamente.
data Pergunta = Sim | Nao

-- a) pergNum : recebe via parâmetro uma Pergunta . Retorna 0 para Nao e 1 para Sim .
pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

-- b) recebe via parâmetro uma lista de Perguntas , e retorna 0 s e 1 s correspondentes aos
-- constructores contidos na lista.
listPergs :: [Pergunta] -> [Int]
-- listPergs xs = [ pergNum x | x <- xs ] -- list comprehension
-- listPergs xs = map pergNum xs -- sem ETA reducao com map
listPergs = map pergNum -- com ETA reducao e map

-- c) and' : recebe duas Perguntas como parâmetro e
-- retorna a tabela verdade do and lógico, usando Sim como verdadeiro e Nao como falso.

-- PRIMEIRA OPÇÃO (sem pattern matching)
-- binarioParaBool :: Int -> Bool
-- binarioParaBool x = x == 1

-- and' :: Pergunta -> Pergunta -> Bool
-- and' p1 p2 = (binarioParaBool $ pergNum p1) && (binarioParaBool $ pergNum p2)

-- SEGUNDA OPÇÃO (com pattern matching)
and' :: Pergunta -> Pergunta -> Bool
and' Sim Sim = True
and' Sim Nao = False
and' Nao Sim = False
and' Nao Nao = False

-- d) or' : idem ao anterior, porém deve ser usado o ou lógico.

-- e) not' : idem aos anteriores, porém usando o not lógico.

-- 3.6)  Faça um novo tipo chamado Mes , que possui como valores todos os meses do ano. Implemente:
data Mes = Janeiro | Fevereiro | Marco | Abril
        | Maio | Junho | Julho
        | Agosto | Setembro | Outubro
        | Novembro | Dezembro deriving Show

-- a) A função checaFim , que retorna o número de dias que cada mês possui (considere fevereiro tendo 28 dias).

diasMes :: Mes -> Int
diasMes Janeiro = 30
diasMes Fevereiro = 28
diasMes Marco = 31
diasMes Abril = 31
diasMes Maio = 31
diasMes Junho = 30
diasMes Julho = 30
diasMes Agosto = 30
diasMes Setembro = 30
diasMes Outubro = 30
diasMes Novembro = 30
diasMes Dezembro = 30

checaFim :: [Mes] -> [Int]
checaFim xs = map diasMes xs

-- [Janeiro, Abril, Novembro]
-- [30, 31, 30]

-- b) A função prox , que recebe um mês atual e retorna o próximo mês.
prox :: Mes -> Mes
prox Janeiro = Fevereiro
prox Fevereiro = Marco
prox Marco = Abril
prox Abril = Maio
prox Maio = Junho
prox Junho = Julho
prox Julho = Agosto
prox Agosto = Setembro
prox Setembro = Outubro
prox Outubro = Novembro
prox Novembro = Dezembro
prox Dezembro = Janeiro

-- c) A função estacao , que retorna a estação do ano de acordo com o mês e com o hemisfério.
data Hemisferio = Norte | Sul | Leste | Oeste

data Estacao = Sol | Chuva  deriving Show

estacao :: Mes -> Hemisferio -> Estacao
estacao Janeiro Norte = Chuva
estacao Janeiro Sul = Sol
estacao Janeiro Leste = Chuva
estacao Janeiro Oeste = Sol
estacao _ _ = Sol

------

-- 3.5) Sabe-se que as unidades imperiais de comprimento podem ser Inch , Yard ou Foot (há outras ignoradas aqui). 
-- Sabe-se que 1in=0.0254m , 1yd=0.9144m , 1ft=0.3048 . Faça a função converterMetros que recebe a unidade imperial 
-- e o valor correspondente nesta unidade. Esta função deve retornar o valor em metros.

data UnidadeImperial = Inch | Yard | Foot
-- 1in=0.0254m , 1yd=0.9144m , 1ft=0.3048
converterMetros :: UnidadeImperial -> Double -> Double
converterMetros Inch valor = valor / 39.37
converterMetros Yard valor = valor / 1.094
converterMetros Foot valor = valor / 3.281

-- Implemente também a função converterImperial , que recebe um valor em metros e a unidade de conversão.
-- Esta função deve retornar o valor convertido para a unidade desejada.
converterImperial :: Double -> UnidadeImperial -> Double
converterImperial valor Inch = valor * 39.37
converterImperial valor Foot = valor * 3.281
converterImperial valor Yard = valor * 1.094
