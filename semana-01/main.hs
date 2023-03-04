-- 2.1) a) P.G
[ round $ 1 * (11 ** (x-1)) | x <- [1..7] ]

-- 2.1) b)
[ x | x <- [1..40], mod x 4 /= 0 ]

-- 2.1) c)
[ 'A':x:"BB" | x <- ['a'..'g'] ]

-- 2.1) d) P.A
[  5 + (x - 1) * 3 | x <- [1..13] ]

-- 2.1) e)
[ 1/2 ** x | x <- [0..5] ]

-- 2.1) f) P.A
[ 1 + (x - 1) * 9 | x <- [1..8]]

-- 2.1) g) P.A
-- ! função "length" -> recebe uma lista e retorna a quantidade de elementos
-- ! utilizar o "==" para testar os resultados
[ 2 + (x - 1) * 2 | x <- [1..15], x `notElem` [3,7,10,13]]

-- 2.1) h)
'@' : [ x | x <- ['A'..'L'], x `notElem` ['B', 'F', 'H', 'I', 'K']]



