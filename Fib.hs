import BigNumber
--Uma função recursiva, fibRec
fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n-1) + fibRec (n-2)


-- versão otimizada da função anterior, fibListafib::Int->[Int]
fibListaux :: Integral b => b -> [b]
fibListaux n = map fibRec [0..n]

fibList :: Int -> Int
fibList n = fibListaux n !! n


--gerar uma lista infinita com todos os números de Fibonacci e retornar o elemento de ordem n,  fibListaInfinita
fibListaInfinitaaux :: [Integer]
fibListaInfinitaaux = 0 : 1 : zipWith (+) fibListaInfinitaaux (tail fibListaInfinitaaux)

fibListaInfinita :: Int -> Integer
fibListaInfinita a = fibListaInfinitaaux !! a

fibRecBN :: BigNumber -> BigNumber
fibRecBN [1] = [1]
fibRecBN [0] = [0]
fibRecBN a = somaBN (fibRecBN (subBN a [1])) (fibRecBN(subBN a [2]))


-- versão otimizada da função anterior, fibLista
fibListaBN :: Int -> [BigNumber] -> BigNumber
fibListaBN i lista = head(drop i [fibRecBN x|x<-lista])

-- EXERCICIO 3
-- Agora com BN


{--

fibListaBN ::
fibListaBN = 


fibListaInfinitaBN ::
fibListaInfinitaBN =

--}