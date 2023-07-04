--exencicios lista 01

--ex 01
distancia_dois_pontos:: (Float,Float) -> (Float,Float) -> Float
distancia_dois_pontos (x1,y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2-y1)^2)

--ex 02
ano_bissexto:: Int -> Bool 
ano_bissexto ano =  ano `mod` 4 == 0

--ex04

par:: Int -> Bool
par x = x `mod` 2 == 0

--ex05
conceito :: Float -> Char
conceito x | x < 4 = 'E'
           | x < 5.99 = 'D'
           | x < 7.49 = 'C'
           | x < 8.99 = 'B'
           | x > 9 = 'A'

-- exercicios 3
-- 01
type Nome = String
type Idade = Int
type Peso = Float
type Esporte = String
type Pessoa = (Nome, Idade, Peso, Esporte)

-- 02
bancoDeDados:: Int -> Pessoa
bancoDeDados id | id == 1 = ("Daniel", 24, 74.0, "Futebol")
                | id == 2 = ("Gabriel", 23, 80.0, "Basquete")
                | otherwise = ("", 0, 0.0, "")
--03
pessoa :: Pessoa -> String
pessoa (n, i, p, e) = n

--04
mais_nova :: Pessoa -> Pessoa -> Pessoa
mais_nova (n1, i1, p1, e1) (n2, i2, p2, e2)
  | i1 < i2 = (n1, i1, p1, e1)
  | otherwise = (n2, i2, p2, e2)

mais_leve :: Pessoa -> Pessoa -> Pessoa
mais_leve (n1, i1, p1, e1) (n2, i2, p2, e2)
  | p1 < p2 = (n1, i1, p1, e1)
  | otherwise = (n2, i2, p2, e2)

--recursão

--01 
potencia::Integer-> Integer -> Integer
potencia b 1 = b
potencia b x = b * potencia b (x-1)

--02

e_par:: Integer -> Bool
e_par 0 = True
e_par 1 = False
e_par num = e_par (num - 2)

-- 03

somatorio:: Int -> Int 
somatorio 0 = 0
somatorio n = n + somatorio (n -1)

--04
somaDigitos:: Int -> Int
somaDigitos 0 = 0
somaDigitos x = x `mod` 10 + somaDigitos (x `div` 10 )


--Listas 

-- 01
alfabeto :: [Char]
alfabeto = ['a'..'z']

--02
decrescenteDuzentos :: [Int]
decrescenteDuzentos = [200,199..0]

--03 inverso

inverso:: [a] -> [a]
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

--04 n primeiro elementos lista
divide:: [a] -> Int -> [a]
divide _ 0 = [] 
divide (x:xs) n = x: divide xs (n-1)

primeiro:: [a] -> Int -> [a]
primeiro lista n = if length lista <= n
    then lista
    else divide lista n
-- 05 remover n primeiros

remove:: [a] -> Int -> [a]
remove [] n = []
remove lista 0 = lista
remove (x:xs) n = remove xs (n-1)

--06 remover ultimo
removeUltimo:: [a] -> [a]
removeUltimo (_:[]) = []
removeUltimo (x:xs) = x:removeUltimo xs 

--07 remove n ultimos

removeUltimos:: [a] -> Int -> [a]
removeUltimos (x:xs) n = if length xs == (n - 1)
    then []
    else x:removeUltimos xs n 
--08 remove n-ésimo elemento da lista

removeElemento:: [a] -> Int -> [a]
removeElemento [] n = [] 
removeElemento (x:xs) 1 = xs
removeElemento (x:xs) n = x: removeElemento xs (n-1) 

--  Listas de compreção
--01 retorna lista de inteiro pares ou impares

filtraLista:: [Int] -> Int -> [Int]
filtraLista xs opc = if opc == 1 
    then [x | x <- xs, (x `mod` 2 == 0) ]
    else [x | x <- xs, (x `mod` 2 /= 0) ]

-- 02 retornar a soma da tupla
somaTupla:: [(Int,Int)] -> [Int]
somaTupla lista = [ x + y | (x,y) <- lista]

--03 combinação em pares
combinaPares:: [Int] -> [Int]-> [(Int,Int)]
combinaPares a b = [(x,y) | x <- a , y <- b]

--04 remover caracter
removerCaracter:: String-> Char -> String
removerCaracter s c = [x | x <- s , x /= c]

-- 05 alunos de nota acima de 8
baseDeDados :: [(Int, String,  Float)]
baseDeDados = [ (1, "André", 10.0), (2, "Carlos", 6.8), (3, "Maurício", 7.0)]
alunosOito:: [Int]
alunosOito = [ c | (c, a , n) <- baseDeDados, n > 8.0]

-- 06 Like de nomes com inicial
like:: [String] -> Char -> [String]
like lista n = [x:xs | (x:xs) <- lista, x == n ]

-- funções de alta ordem 

--01
tamanhoStrins:: [String] -> [Int]
tamanhoStrins lista = map (\x -> (length x)) lista

--02 takeWhiles
takeWhiles :: (a -> Bool) -> [a] -> [a]
takeWhiles p [] = []
takeWhiles p (x:xs)
    | p x = x : takeWhiles p xs
    | otherwise = takeWhiles p xs

--03

--04 increnenta 

soma :: Int -> Int -> Int
soma x y = x + y

incrementa :: Int -> Int
incrementa = soma 1

-- 05 incrementa lista

incrementaLista :: [Int] -> [Int]
incrementaLista = map (soma 1)

-- 06 

intToFloat :: [Int] -> [Float]
intToFloat = map fromIntegral

--Proximo caracterdo alfabeto
proximoCaractere :: Char -> Char
proximoCaractere x = procuraProximo alfabeto x

procuraProximo:: Eq a => [a] -> a -> a
procuraProximo (x:xs) a 
            | a == x = retornaPorIndice xs 1 
            | otherwise = procuraProximo xs a
retornaPorIndice:: [a] -> Int -> a
retornaPorIndice (x:xs) 0 = x
retornaPorIndice (x:xs) n = retornaPorIndice xs (n-1)

quantidadeCaracter:: [String] -> [Int]
quantidadeCaracter = map (\x -> (length x))


