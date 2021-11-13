module BigNumber where
import Data.List

-- EXERCICIO 2
-- Uma definição do tipo BigNumber
type BigNumber = [Int]

--scanner, que converte uma string em big-number
scanner :: String -> BigNumber 
scanner s = map(read . (:"")) s  :: BigNumber 


-- output, que converte um big-number em string
output :: BigNumber -> String
output b = (concat (map show (b)))


{--
-- para somar dois big-numbers.
somaBN ::  BigNumber -> BigNumber -> BigNumber
somaBN a b = reverse [x+y|(x,y)<-zip a b] --ainda por acabar


-- subtrair dois big-numbers
subBN ::  BigNumber -> BigNumber -> BigNumber


--multiplicar dois big-numbers.
mulBN ::  BigNumber -> BigNumber -> BigNumber


--a divisão inteira de dois big-numbers. A divisão deverá retornar um par “(quociente, resto)”
divBN :: BigNumber -> BigNumber -> (BigNumber,BigNumber)


-- EXERCICIO 5
safeDivBN ::
safeDivBN =
--}