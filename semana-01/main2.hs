-- 2.2)
tamanhoStringEPar :: String -> Bool
tamanhoStringEPar xs = even $ length xs

-- 2.3)
reverteListaStrings :: [String] -> [String]
reverteListaStrings xs = reverse [ reverse x | x <- xs ]

-- 2.4)
-- excluir palavras de tamanho par
tamanhoDaStringEImpar :: String -> Bool
tamanhoDaStringEImpar x = odd $ length x

removePalavrasDeTamanhoPar :: [String] -> [Int]
removePalavrasDeTamanhoPar xs = [ length x | x <- xs, tamanhoDaStringEImpar x ]

-- ! 2.5) proxima monitoria ou durante a semana

-- 2.6)
verificaPalindromo :: String -> Bool
verificaPalindromo xs = xs == reverse xs

-- 2.7)
recebeInteiroRetornaTupla :: Int -> (Int, Int, Int, Int)
recebeInteiroRetornaTupla x = ( x*2, x*3, x*4, x*5 )