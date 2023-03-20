-- Exercícios passados na monitoria anterior

-- 3.3) Implemente uma função que simule o vencedor de uma partida de pedra, papel e tesoura usando tipos criados.
-- Casos de empate devem ser considerados em seu tipo.

data JokenPo = Pedra | Papel | Tesoura deriving Show

pedraPapelTesoura :: JokenPo -> JokenPo -> String
pedraPapelTesoura Pedra Tesoura = "Jogador 1 venceu"
pedraPapelTesoura Pedra Papel = "Jogador 2 venceu"
pedraPapelTesoura Tesoura Papel = "Jogador 1 venceu"
pedraPapelTesoura Tesoura Pedra = "Jogador 2 venceu"
pedraPapelTesoura Papel Pedra = "Jogador 1 venceu"
pedraPapelTesoura Papel Tesoura = "Jogador 2 venceu"
pedraPapelTesoura _ _ = "Empate!"

-- 3.17) Faça o tipo Cripto que possua dois values constructors Mensagem e Cifrado , ambos com um campo String
-- e um value constructor Erro . Faça as funções encriptar e decriptar , seguindo cada exemplo a seguir. 
-- Prelude> encriptar (Mensagem "FATEC")

data Cripto = Mensagem String | Cifrado String | Erro deriving Show

encriptar :: Cripto -> Cripto
encriptar (Mensagem msg) = Cifrado (map succ msg)
encriptar _ = Erro

decriptar :: Cripto -> Cripto
decriptar (Cifrado c) = Mensagem (map pred c)
decriptar _ = Erro

-- ghci> encriptar (Mensagem "FATEC")
-- Cifrado "GBUFD"
-- ghci> decriptar (Cifrado "GBUFD")
-- Mensagem "FATEC"
