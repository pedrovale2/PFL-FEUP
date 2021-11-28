{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
module BigNumber where
import Data.List (inits)
import Data.Text (append)
import GHC.Integer.GMP.Internals (BigNat)
import Data.List (transpose)

-- EXERCICIO 2
-- Uma definição do tipo BigNumber
type BigNumber = [Int]

--scanner, que converte uma string em big-number
scanner :: String -> BigNumber
scanner s
    |take 1 s == "-" = changeSignal(map(read . (:"")) (drop 1 s))
    |otherwise = map(read . (:"")) s

--funções auxiliares do output
addminus :: BigNumber -> BigNumber
addminus a = head a * (-1) : drop 1 a

checksign :: BigNumber -> BigNumber
checksign [0] = [0]
checksign [] = []
checksign (x:xs)
            | x < 0 = addminus(changeSignal (x:xs))
            | otherwise = [x] ++ checksign xs

-- output, que converte um big-number em string
output :: BigNumber -> String
output b = concat (map show (checksign (b)))



--introduzir 0 na lista ate ficar com n elementos
fillwithzero :: Num a => Int -> [a] -> [a]
fillwithzero n a = if length a >= n then a else fillwithzero n (a ++ [0])


--limpar os 0 a esquerda
clearLeftZeros :: BigNumber -> BigNumber
clearLeftZeros [0] = [0]
clearLeftZeros [] = []
clearLeftZeros (x:xs)
                | x == 0 = [] ++ clearLeftZeros xs
                | otherwise = [x] ++ xs

-- para somar dois big-numbers.
somaAux ::  BigNumber -> BigNumber -> BigNumber
somaAux = zipWith (+)

--preencher as filas com zeros fazendo com que tenham o mesmo tamanho 
--fazer a somas das listas revertendo-as
--reverter voltando ao estado original com as somas
teste :: BigNumber -> BigNumber -> BigNumber
teste a b = reverse $ somaAux (fillwithzero (length b) (reverse a)) (fillwithzero (length a) (reverse b))

--tratar do resultado final utilizando carry quando o resultado é postivo
carryspos :: Int -> BigNumber -> BigNumber
carryspos 1 [] = [1]
carryspos 0 [] = []
carryspos (-1) [] = []
carryspos n (x:xs)
                | x + n >= 10 =  mod (x + n) 10 : carryspos 1 xs
                | x + n < 0 = (10 + x + n) : carryspos (-1) xs
                | otherwise = (x + n) : carryspos 0 xs


--tratar do resultado final utilizando carry quando o resultado é negativo
carrysneg :: Int -> BigNumber -> BigNumber
carrysneg (-1) [] = [-1]
carrysneg 0 [] = []
carrysneg 1 [] = []
carrysneg n (x:xs)
                | x + n > 0 = (x+n - 10) : carrysneg 1 xs
                | x + n <= -10 = mod (x + n) (-10) : carrysneg (-1) xs
                | otherwise = (x + n) : carrysneg 0 xs

--verificar se o resultado e negativo ou positivo, chamando consecutivamente as funções respetivas para tratar do resultado
checksignal :: BigNumber -> BigNumber
checksignal [0] = [0]
checksignal (x:xs)
            | x > 0 = carryspos 0 (reverse (x:xs))
            | x < 0 = carrysneg 0 (reverse (x:xs))
            | otherwise = checksignal xs

--soma final
somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN a b = clearLeftZeros( reverse( checksignal  (teste a b)))


-- Transforma a subtração numa adiçao
changeSignal :: BigNumber -> BigNumber
changeSignal = map (\ x -> - x)

--sub Final
subBN :: BigNumber -> BigNumber -> BigNumber
subBN a b = clearLeftZeros (reverse (checksignal (teste a (changeSignal b))))


--multiplicar dois big-numbers.
--mulBN ::  BigNumber -> BigNumber -> BigNumber

--faz multiplicação
multAux :: BigNumber -> BigNumber -> BigNumber
multAux = zipWith (*)

--multiplica um numero por uma lista
multiSingular :: Int -> BigNumber -> BigNumber
multiSingular n x = multAux (cycle ([n])) x


--Preencher a lista de zeros
prepListas :: Int -> BigNumber -> BigNumber
prepListas n a = fillwithzero n (reverse a)

-- cria uma lista de BigNumbers com as multiplicaçoes feitas
-- multiTotal [1,2,3] [4,5] => [[4*1=4,4*2=8,4*3=12], [5*1=5,5*2=10,5*3=15]]
multiTotal :: BigNumber -> BigNumber -> [BigNumber]
multiTotal _ [] = []
multiTotal [] _ = []
multiTotal a (x:xs) = [multiSingular x a ] ++ multiTotal a xs

-- prepara os bigNumbers colocando-os com o mesmo numero de elementos e invoca multiTotal para termos as multiplicaçoes
multiF :: BigNumber -> BigNumber -> [BigNumber]
multiF a b = multiTotal (prepListas (length b) a) (prepListas (length a) b)

-- adiciona n zeros a lista x
addZeros :: BigNumber -> Int -> BigNumber
addZeros x n = x ++ take n (cycle[0])

--Preencher de zeros
processList :: [BigNumber] -> Int -> [BigNumber]
processList x 0 = x
processList (x:xs) n = [fillLeftZero n x] ++ processList xs (n-1)

--coloca os BigNumber com o mesmo tamanho
sameSize :: [BigNumber] -> Int -> [BigNumber]
sameSize [] n = []
sameSize (x:xs) n = processList2 [x] n ++ sameSize xs n

-- adiciona n 0 a x, a esquerda
fillLeftZero :: Num a => Int -> [a] -> [a]
fillLeftZero n x = take n (cycle[0]) ++ x


processList2 :: [BigNumber] -> Int -> [BigNumber]
processList2 x 0 = x
processList2 [] _ = []
processList2 (x:xs) n = [addZeros x ( n -(length x))] ++ processList2 xs (n)

--preenche ate ter o mesmo tamanho todos os BigNumbers
fillit :: [BigNumber] -> [BigNumber]
fillit [] = []
fillit (x:xs) = sameSize (x:xs) (length x)


--soma todos os numeros da lista
sumMult :: [BigNumber] -> BigNumber
sumMult [] = []
sumMult (x:xs) = [sum x] ++ sumMult xs


--trata do resultado final se for positivo
carrysMultPos :: Int -> BigNumber -> BigNumber
carrysMultPos x [] = [x]
carrysMultPos n (x:xs)
                    | x + n >= 10 = [mod (x + n) 10] ++ carrysMultPos (div (x + n) 10) xs
                    | otherwise = [(x + n)] ++ carrysMultPos 0 xs

--trata do resultado final se for negativo
carrysMultNeg :: Int -> BigNumber -> BigNumber
carrysMultNeg x [] = [x]
carrysMultNeg n (x:xs)
                    | x + n <= -10 = [mod (x + n) (-10)] ++ carrysMultNeg (-(div (x + n) (-10))) xs
                    | otherwise = [x + n] ++ carrysneg 0 xs


-- faz a multiplicacao 
multiBefore :: BigNumber -> BigNumber -> [BigNumber]
multiBefore a b = reverse (multiF a b)

-- processa o resultado da multiplicacao
multiAux :: [BigNumber] -> [BigNumber]
multiAux (x:xs ) = processList (x:xs) (length x - 1)

-- faz a soma de todos os elementos 
multiAux1 :: BigNumber -> BigNumber -> BigNumber
multiAux1 a b = sumMult(transpose(fillit(multiAux(multiBefore a b))))


--verifical se é negativo ou positivo
checkSignalMul :: BigNumber -> BigNumber
checkSignalMul [0] = [0]
checkSignalMul (x:xs)
            | x > 0 = carrysMultPos 0 (reverse (x:xs))
            | x < 0 = carrysMultNeg 0 (reverse (x:xs))
            | otherwise = checkSignalMul xs


--Multiplicaçao final
mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN a b = clearLeftZeros (reverse (checkSignalMul (reverse (multiAux1 a b))))

-- compara se x e maior que y e retorna true se for
maiorque :: Ord a => [a] -> [a] -> Bool
maiorque [] _  = False  
maiorque _ [] = True 
maiorque (x:xs) (y:ys)
            | length (x:xs) < length (y:ys) = False
            | length (x:xs) == length (y:ys) && x < y = False
            | length (x:xs) == length (y:ys) && x == y = maiorque xs ys
            | otherwise = True

-- compara se x e maior ou igual que y e retorna true se for
maiorouigualque :: Ord a => [a] -> [a] -> Bool
maiorouigualque _ [] = True 
maiorouigualque [] _  = False  
maiorouigualque (x:xs) (y:ys)
            | length (x:xs) > length (y:ys) = True
            | length (x:xs) == length (y:ys) && x > y = True
            | length (x:xs) == length (y:ys) && x == y = maiorouigualque xs ys
            | otherwise = False

-- faz a divisao 
-- divaux [1,2,3] [2,3] = [5]
divaux :: BigNumber -> BigNumber -> Int -> BigNumber
divaux a b n 
            | maiorque (mulBN b [n+1]) a  = mulBN [n] [1]
            | otherwise = divaux a b (n+1) 

-- arranja o resto
-- arranjarresto [1,2,3] [2,3] [5] = [8]
arranjarresto :: BigNumber -> BigNumber -> BigNumber -> BigNumber
arranjarresto a b n = subBN a (mulBN b n)


adicionarelem :: (Ord a, Num t, Eq t) => [a] -> [a] -> t -> [a]
adicionarelem a b n
                | maiorouigualque (retirarelem a n) b = retirarelem a n
                | otherwise = adicionarelem a b (n+1)

-- arranja os elementos a ser divididos
-- aux123 [8,2,3,4] [2,3] = [8,2]
aux123 :: Ord a => [a] -> [a] -> [a]
aux123 a b = adicionarelem a b (length b)

-- vai buscar o resto
aux1234 :: BigNumber -> BigNumber -> BigNumber
aux1234 a b = arranjarresto (aux123 a b) b (divaux (aux123 a b) b 1)

-- trata do resultado da divisao depois da primeira iteracao
arranjarresultado :: BigNumber -> BigNumber -> BigNumber -> BigNumber
arranjarresultado a b resto = divaux (auxiliarDividendo a b resto) b 1 

-- trata do resultado da divisao na primeira iteracao
arranjarresultadoVazio :: BigNumber -> BigNumber -> BigNumber -> BigNumber
arranjarresultadoVazio a b resto = divaux (auxiliarDividendoVazio a b resto) b 1 


-- FUncoes auxiliares da divisao a partir da primeira iteraçao
auxiliarDividendo :: Ord a => [a] -> [a] -> [a] -> [a]
auxiliarDividendo a b [] = a
auxiliarDividendo a b (x:xs)
                            | maiorouigualque b a = auxiliarDividendo (a ++[x]) b xs  
                            | otherwise = a
                                 
auxiliarCarry:: Ord a => [a] -> [a] -> [a] -> [a]
auxiliarCarry a b [] = []
auxiliarCarry a b (x:xs)
                        | maiorouigualque b a = auxiliarCarry (a ++[x]) b xs    
                        | otherwise = (x:xs)         


-- FUncoes auxiliares da divisao na primeira iteraçao
auxiliarDividendoVazio :: Ord a => [a] -> [a] -> [a] -> [a]
auxiliarDividendoVazio [] b resto = resto
auxiliarDividendoVazio (x:xs) b resto
                                | maiorouigualque b resto = auxiliarDividendoVazio xs b (resto ++ [x])   
                                | otherwise = resto
                                 
auxiliarCarryVazio :: Ord a => [a] -> [a] -> [a] -> [a]
auxiliarCarryVazio [] b resto = []
auxiliarCarryVazio (x:xs) b resto
                            | maiorouigualque b resto = auxiliarCarryVazio xs b (resto ++ [x])   
                            | otherwise = (x:xs)    


retirarelem :: (Eq t, Num t) => [a] -> t -> [a]
retirarelem _ 0 = []
retirarelem (x:xs) n = [x] ++ retirarelem xs (n-1)


-- compoe todas as funcoes
compor :: BigNumber -> BigNumber-> BigNumber -> BigNumber
compor a b resto 
                | resto == [] && maiorque b a = [] -- ultima iteraçao
                | resto == [] && maiorque a b = arranjarresultadoVazio a b resto ++ compor (aux1234 (auxiliarDividendoVazio a b resto) b) b (auxiliarCarryVazio a b resto)  --2ª ate a penultima
                | otherwise = arranjarresultado a b resto ++ compor (aux1234 (auxiliarDividendo a b resto) b) b (auxiliarCarry a b resto)  --vazio primeira iteraçao


-- trata do resto
restoaux :: BigNumber -> BigNumber -> BigNumber
restoaux a b = arranjarresto a b (compor a b [])

divBNaux :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBNaux a b = (compor a b [], restoaux a b) 


divBN :: BigNumber -> BigNumber-> (BigNumber, BigNumber)
divBN a b
        | b == [0] = ([0], [0])
        | maiorque b a = ([0],a)
        | otherwise = divBNaux a b


safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN a b
            | maiorque (clearLeftZeros b) [0]= Just (divBN a b)
            | otherwise = Nothing 