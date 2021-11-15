module BigNumber where
import Data.List
import Data.Text (append)

-- EXERCICIO 2
-- Uma definição do tipo BigNumber
type BigNumber = [Int]

--scanner, que converte uma string em big-number
scanner :: String -> BigNumber 
scanner s = map(read . (:"")) s  :: BigNumber 


-- output, que converte um big-number em string
output :: BigNumber -> String
output b = (concat (map show (b)))

fillwithzero :: Num a => Int -> [a] -> [a]
fillwithzero n a = if length a >= n then a else fillwithzero n (a ++ [0])

-- para somar dois big-numbers.
somaAux ::  BigNumber -> BigNumber -> BigNumber
somaAux a b =  [x+y|(x,y)<-zip a b] --ainda por acabar

teste :: BigNumber -> BigNumber -> BigNumber
teste a b = somaAux (fillwithzero (length b) (reverse a)) (fillwithzero (length a) (reverse b))

carrys :: Int -> BigNumber -> BigNumber
carrys 1 [] = [1]
carrys 0 [] = []
carrys n (x:xs) = if x + n >= 10 then [mod (x + n) 10] ++ carrys 1 xs else [x + n] ++ carrys 0 xs

somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN a b = reverse (carrys 0 (teste a b))


-- subtrair dois big-numbers
subAux ::  BigNumber -> BigNumber -> BigNumber
subAux a b =  [x-y|(x,y)<-zip a b] --ainda por acabar

teste2 :: BigNumber -> BigNumber -> BigNumber
teste2 a b = subAux (fillwithzero (length b) (reverse a)) (fillwithzero (length a) (reverse b))

carrys2 :: Int -> [Int] -> [Int]
carrys2 0 [] = []
carrys2 n (x:xs) = if x < 0 then [x - n + 10] ++ carrys2 1 xs else [x - n] ++ carrys2 0 xs

subBN :: BigNumber -> BigNumber -> BigNumber
subBN a b = reverse (carrys2 0 (teste2 a b))


{--

--multiplicar dois big-numbers.
mulBN ::  BigNumber -> BigNumber -> BigNumber


--a divisão inteira de dois big-numbers. A divisão deverá retornar um par “(quociente, resto)”
divBN :: BigNumber -> BigNumber -> (BigNumber,BigNumber)


-- EXERCICIO 5
safeDivBN ::
safeDivBN =
--}