import Data.Char (isDigit)

avalia::String->Float
avalia exp = string_to_float ((ini (extrairCaracteres exp)) !! 0) -- !! 0 pega o primeiro elemento da lsita de retorno do 'ini'

ini::[String]->[String]
ini exp | (elem "(" exp) = ini (executa_parentese exp)
        | (elem "*" exp) = ini (replace_conta exp (multiplica exp (pega_index_operacao exp "*")) "*")
        | (elem "/" exp) = ini (replace_conta exp (divide exp (pega_index_operacao exp "/")) "/")
        | (elem "+" exp) = ini (replace_conta exp (soma exp (pega_index_operacao exp "+")) "+")
        | (elem "-" exp) = ini (replace_conta exp (subtrai exp (pega_index_operacao exp "-")) "-")
        | otherwise = exp

replace_conta::[String]->Float->String->[String]
replace_conta exp valor operador = replace exp ((pega_index_operacao exp operador) - 1) ((pega_index_operacao exp operador) + 1) (float_to_string valor)

executa_parentese::[String]->[String]
executa_parentese exp = replace exp (pega_index_operacao exp "(") (pega_fechamento_parentese exp ((pega_index_operacao exp "(") + 1) 0) ((executa_parentese_interno exp) !! 0)
executa_parentese_interno::[String]->[String]
executa_parentese_interno exp = ini (sublista exp ((pega_index_operacao exp "(") + 1) ((pega_fechamento_parentese exp ((pega_index_operacao exp "(") + 1) 0) - 1))

pega_index_operacao::[String]->String->Int
pega_index_operacao exp op = pega_index_operacao_aux exp op 0 (length exp)

-- valores (Expreção, String da operação, Indice atual, Indice máximo)
pega_index_operacao_aux::[String]->String->Int->Int->Int
pega_index_operacao_aux exp op actual max | actual == max = -1
                                          | op == (exp !! actual) = actual
                                          | otherwise = pega_index_operacao_aux exp op (actual + 1) max

-- operações (pega o valor anterior do indice que corresponde ao caracter de eperação
-- e o valor posterior, tranforma e float e executa a ação)
multiplica::[String]->Int->Float
multiplica exp index = (string_to_float (exp !! (index - 1))) * (string_to_float (exp !! (index + 1)))

divide::[String]->Int->Float
divide exp index = (string_to_float (exp !! (index - 1))) / (string_to_float (exp !! (index + 1)))

soma::[String]->Int->Float
soma exp index = (string_to_float (exp !! (index - 1))) + (string_to_float (exp !! (index + 1)))

subtrai::[String]->Int->Float
subtrai exp index = (string_to_float (exp !! (index - 1))) - (string_to_float (exp !! (index + 1)))

-- transformar string em float
string_to_float :: String -> Float
string_to_float str = read str :: Float

float_to_string::Float->String
float_to_string f = show f


--  pega_fechamento_parentese (words "1 + 2 - ( ( 10 - 1 ) * ( 9 / 3 ))") (pega_index_operacao "1 + 2 - ( ( 10 - 1 ) * ( 9 / 3 ))" '(')

pega_fechamento_parentese::[String]->Int->Int->Int
pega_fechamento_parentese exp index para_fechar | ((exp !! index) == ")") && (para_fechar == 0) = index
                                                | ((exp !! index) == ")") && (para_fechar > 0) = pega_fechamento_parentese exp (index + 1) (para_fechar - 1)
                                                | ((exp !! index) == "(") = pega_fechamento_parentese exp (index + 1) (para_fechar + 1)
                                                | otherwise = pega_fechamento_parentese exp (index + 1) para_fechar
-- retorna sublista entre ()
sublista :: [String] -> Int -> Int -> [String]
sublista lista i j = take ((j - i) + 1) (drop i lista)

-- replace sobrescreve a execução entre () ou a conta feita
replace :: [String] -> Int -> Int -> String -> [String]
replace lista inicio fim elemento = prefixo ++ [elemento] ++ sufixo
  where
    prefixo = take inicio lista -- retorna os primeiros elementos da lista definido pelo indice inicio 
    sufixo = drop (fim + 1) lista -- remove os primeiros (fim +1) elementos da lista

-- retornar uma lista de caracteres lidos
extrairCaracteres :: String -> [String]
extrairCaracteres [] = []
extrairCaracteres (x:xs)
    | elem x "+-*/()" = [x] : extrairCaracteres xs
    | isDigit x || x == '.' = (x : takeWhile (\c -> isDigit c || c == '.') xs) : extrairCaracteres (dropWhile (\c -> isDigit c || c == '.') xs)
    | otherwise = extrairCaracteres xs