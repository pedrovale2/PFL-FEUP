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
scanner s = map(read . (:"")) s  :: BigNumber


-- output, que converte um big-number em string
output :: BigNumber -> String
output b = concat (map show (b))

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


-- subtrair dois big-numbers
subAux ::  BigNumber -> BigNumber -> BigNumber
subAux a b =  [x-y|(x,y)<-zip a b] --ainda por acabar


--igual a teste1 so que para negativo
teste2 :: BigNumber -> BigNumber -> BigNumber
teste2 a b = subAux (fillwithzero (length b) (reverse a)) (fillwithzero (length a) (reverse b))

carrys2 :: Int -> [Int] -> [Int]
carrys2 0 [] = []
carrys2 n (x:xs) = if x < 0 then (x - n + 10) : carrys2 1 xs else (x - n) : carrys2 0 xs

changeSignal :: BigNumber -> BigNumber
changeSignal = map (\ x -> - x)

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


multiTotal :: BigNumber -> BigNumber -> [BigNumber]
multiTotal _ [] = []
multiTotal [] _ = []
multiTotal a (x:xs) = [multiSingular x a ] ++ multiTotal a xs

multiF :: BigNumber -> [Int] -> [BigNumber]
multiF a b = multiTotal (prepListas (length b) a) (prepListas (length a) b)

addZeros :: BigNumber -> Int -> BigNumber
addZeros x n = x ++ take n (cycle[0])

--Preencher de zeros
processList :: [BigNumber] -> Int -> [BigNumber]
processList x 0 = x
processList (x:xs) n = [fillLeftZero n x] ++ processList xs (n-1)

sameSize :: [BigNumber] -> Int -> [BigNumber]
sameSize [] n = []
sameSize (x:xs) n = processList2 [x] n ++ sameSize xs n

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

carrysMultPos :: Int -> BigNumber -> BigNumber
carrysMultPos x [] = [x]
carrysMultPos n (x:xs)
                    | x + n >= 10 = [mod (x + n) 10] ++ carrysMultPos (div (x + n) 10) xs
                    | otherwise = [(x + n)] ++ carrysMultPos 0 xs




carrysMultNeg :: Int -> BigNumber -> BigNumber
carrysMultNeg x [] = [x]
carrysMultNeg n (x:xs)
                    | x + n <= -10 = [mod (x + n) (-10)] ++ carrysMultNeg (-(div (x + n) (-10))) xs
                    | otherwise = [x + n] ++ carrysneg 0 xs


multiBefore :: BigNumber -> BigNumber -> [BigNumber]
multiBefore a b = reverse (multiF a b)

multiAux :: [BigNumber] -> [BigNumber]
multiAux (x:xs ) = processList (x:xs) (length x - 1)

multiAux1 :: BigNumber -> BigNumber -> BigNumber
multiAux1 a b = sumMult(transpose(fillit(multiAux(multiBefore a b))))


--verifical se é negativo ou positivo
checkSignalMul :: BigNumber -> BigNumber
checkSignalMul [0] = [0]
checkSignalMul (x:xs)
            | x > 0 = carrysMultPos 0 (reverse (x:xs))
            | x < 0 = carrysMultNeg 0 (reverse (x:xs))
            | otherwise = checkSignalMul xs


mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN a b = reverse (checkSignalMul (reverse (multiAux1 a b)))


{--             
--a divisão inteira de dois big-numbers. A divisão deverá retornar um par “(quociente, resto)”
divBN :: BigNumber -> BigNumber -> (BigNumber,BigNumber)



-- EXERCICIO 5
safeDivBN ::
safeDivBN =
--}